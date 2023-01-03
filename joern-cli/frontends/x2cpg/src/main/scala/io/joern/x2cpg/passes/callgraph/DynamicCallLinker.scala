package io.joern.x2cpg.passes.callgraph

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method, TypeDecl}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes, PropertyNames}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language._
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.{NodeDb, NodeRef}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters._

/** We compute the set of possible call-targets for each dynamic call, and add them as CALL edges to the graph, based on
  * call.methodFullName, method.name and method.signature, the inheritance hierarchy and the AST of typedecls and
  * methods.
  *
  * This pass intentionally ignores the vtable mechanism based on BINDING nodes but does check for an existing call edge
  * before adding one. It assumes non-circular inheritance, on pain of endless recursion / stack overflow.
  *
  * Based on the algorithm by Jang, Dongseok & Tatlock, Zachary & Lerner, Sorin. (2014). SAFEDISPATCH: Securing C++
  * Virtual Calls from Memory Corruption Attacks. 10.14722/ndss.2014.23287.
  */
class DynamicCallLinker(cpg: Cpg) extends CpgPass(cpg) {

  import DynamicCallLinker._
  // Used to track potential method candidates for a given method fullname. Since our method full names contain the type
  // decl we don't need to specify an addition map to wrap this in. LinkedHashSets are used here to preserve order in
  // the best interest of reproducibility during debugging.
  private val validM = mutable.Map.empty[String, mutable.LinkedHashSet[String]]
  // Used for dynamic programming as subtree's don't need to be recalculated later
  private val subclassCache   = mutable.Map.empty[String, mutable.LinkedHashSet[String]]
  private val superclassCache = mutable.Map.empty[String, mutable.LinkedHashSet[String]]
  // Used for O(1) lookups on methods that will work without indexManager
  private val typeMap = mutable.Map.empty[String, TypeDecl]
  // For linking loose method stubs that cannot be resolved by crawling parent types
  private val methodMap = mutable.Map.empty[String, Method]

  private def initMaps(): Unit = {
    cpg.typeDecl.foreach { typeDecl =>
      typeMap += (typeDecl.fullName -> typeDecl)
    }
    cpg.method
      .filter(m => !m.name.startsWith("<operator>"))
      .foreach { method => methodMap += (method.fullName -> method) }
  }

  /** Main method of enhancement - to be implemented by child class
    */
  override def run(dstGraph: DiffGraphBuilder): Unit = {
    // Perform early stopping in the case of no virtual calls
    if (!cpg.call.exists(_.dispatchType == DispatchTypes.DYNAMIC_DISPATCH)) {
      return
    }
    initMaps()
    // ValidM maps class C and method name N to the set of
    // func ptrs implementing N for C and its subclasses
    for (
      typeDecl <- cpg.typeDecl;
      method   <- typeDecl._methodViaAstOut
    ) {
      val methodName = method.fullName
      val candidates = allSubclasses(typeDecl.fullName).flatMap { staticLookup(_, method) }
      validM.put(methodName, candidates)
    }

    subclassCache.clear()

    cpg.call.filter(_.dispatchType == DispatchTypes.DYNAMIC_DISPATCH).foreach { call =>
      try {
        linkDynamicCall(call, dstGraph)
      } catch {
        case exception: Exception =>
          exception.printStackTrace()
          throw new RuntimeException(exception)
      }
    }

    superclassCache.clear()
  }

  /** Recursively returns all the sub-types of the given type declaration. Does not account for circular hierarchies.
    */
  private def allSubclasses(typDeclFullName: String): mutable.LinkedHashSet[String] = {
    subclassCache.get(typDeclFullName) match {
      case Some(subClasses) => subClasses
      case None =>
        val directSubclasses =
          cpg.typ
            .nameExact(typDeclFullName)
            .flatMap(_.in(EdgeTypes.INHERITS_FROM).asScala)
            .collect { case x: TypeDecl =>
              x.fullName
            }
            .to(mutable.LinkedHashSet)
        // The second check makes sure that set is changing which wouldn't be the case in circular hierarchies
        val totalSubclasses: mutable.LinkedHashSet[String] = if (directSubclasses.isEmpty) {
          directSubclasses ++ mutable.LinkedHashSet(typDeclFullName)
        } else {
          directSubclasses.flatMap(t => allSubclasses(t)) ++ mutable.LinkedHashSet(typDeclFullName)
        }
        subclassCache.put(typDeclFullName, totalSubclasses)
        totalSubclasses
    }
  }

  /** Recursively returns all the super-types of the given type declaration. Does not account for circular hierarchies.
    */
  private def allSuperClasses(typDeclFullName: String): mutable.LinkedHashSet[String] = {
    superclassCache.get(typDeclFullName) match {
      case Some(superClasses) => superClasses
      case None =>
        val directSuperClasses = mutable.LinkedHashSet.from(
          cpg
            .typeDecl(typDeclFullName)
            .inheritsFromTypeFullName
            .flatMap(t => cpg.typeDecl(t).fullName)
            .toSet
        )
        // The second check makes sure that set is changing which wouldn't be the case in circular hierarchies
        val totalSuperClasses: mutable.LinkedHashSet[String] = if (directSuperClasses.isEmpty) {
          directSuperClasses ++ mutable.LinkedHashSet(typDeclFullName)
        } else {
          directSuperClasses.flatMap(t => allSubclasses(t)) ++ mutable.LinkedHashSet(typDeclFullName)
        }
        superclassCache.put(typDeclFullName, totalSuperClasses)
        totalSuperClasses
    }
  }

  /** Returns the method from a sub-class implementing a method for the given subclass.
    */
  private def staticLookup(subclass: String, method: Method): Option[String] = {
    typeMap.get(subclass) match {
      case Some(sc) =>
        sc._methodViaAstOut
          .nameExact(method.name)
          .and(_.signatureExact(method.signature))
          .map(_.fullName)
          .headOption
      case None => None
    }
  }

  private def resolveCallInSuperClasses(call: Call): Boolean = {
    if (!call.methodFullName.contains(":")) return false
    val Array(fullName, signature) = call.methodFullName.split(":")
    val typeDeclFullName           = fullName.replace(s".${call.name}", "")
    val candidateInheritedMethods =
      cpg.typeDecl
        .fullNameExact(allSuperClasses(typeDeclFullName).toArray: _*)
        .astChildren
        .isMethod
        .name(call.name)
        .and(_.signatureExact(signature))
        .fullName
        .l
    if (candidateInheritedMethods.nonEmpty) {
      validM.put(call.methodFullName, mutable.LinkedHashSet.from(candidateInheritedMethods))
      true
    } else {
      false
    }
  }

  @tailrec
  private def linkDynamicCall(call: Call, dstGraph: DiffGraphBuilder): Unit = {
    // This call linker requires a method full name entry
    if (call.methodFullName.equals("<empty>")) return

    validM.get(call.methodFullName) match {
      case Some(tgts) =>
        val callsOut = call.callOut.fullName.toSetImmutable
        tgts.foreach { destMethod =>
          val tgtM = if (cpg.graph.indexManager.isIndexed(PropertyNames.FULL_NAME)) {
            methodFullNameToNode(destMethod)
          } else {
            cpg.method.fullNameExact(destMethod).headOption
          }
          if (tgtM.isDefined && !callsOut.contains(tgtM.get.fullName)) {
            dstGraph.addEdge(call, tgtM.get, EdgeTypes.CALL)
          } else {
            fallbackToStaticResolution(call, dstGraph)
          }
        }
        // If we only find one method to resolve, we can set it to the full name if this is not already the case
        if (tgts.size == 1 && call.methodFullName != tgts.head)
          dstGraph.setNodeProperty(call, PropertyNames.METHOD_FULL_NAME, tgts.head)
      case None if resolveCallInSuperClasses(call) =>
        // Resolve "hidden" inherited method and leave an alias for the result in validM, then retry
        linkDynamicCall(call, dstGraph)
      case None =>
        fallbackToStaticResolution(call, dstGraph)
    }
  }

  /** In the case where the method isn't an internal method and cannot be resolved by crawling TYPE_DECL nodes it can be
    * resolved from the map of external methods.
    */
  private def fallbackToStaticResolution(call: Call, dstGraph: DiffGraphBuilder): Unit = {
    methodMap.get(call.methodFullName) match {
      case Some(tgtM) => dstGraph.addEdge(call, tgtM, EdgeTypes.CALL)
      case None       => printLinkingError(call)
    }
  }

  private def nodesWithFullName(x: String): Iterable[NodeRef[_ <: NodeDb]] =
    cpg.graph.indexManager.lookup(PropertyNames.FULL_NAME, x).asScala

  private def methodFullNameToNode(x: String): Option[Method] =
    nodesWithFullName(x).collectFirst { case x: Method => x }

  @inline
  private def printLinkingError(call: Call): Unit = {
    logger.info(
      s"Unable to link dynamic CALL with METHOD_FULL_NAME ${call.methodFullName} and context: " +
        s"${call.code} @ line ${call.lineNumber}"
    )
  }
}

object DynamicCallLinker {
  private val logger: Logger = LoggerFactory.getLogger(classOf[DynamicCallLinker])
}
