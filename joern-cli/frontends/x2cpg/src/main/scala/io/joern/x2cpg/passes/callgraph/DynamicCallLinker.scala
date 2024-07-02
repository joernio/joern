package io.joern.x2cpg.passes.callgraph

import io.joern.x2cpg.Defines.DynamicCallUnknownFullName
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method, TypeDecl}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes, PropertyNames}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.{NodeDb, NodeRef}

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

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
          throw new RuntimeException(exception)
      }
    }
  }

  /** Recursively returns all the sub-types of the given type declaration. Does account for circular hierarchies.
    */
  private def allSubclasses(typDeclFullName: String): mutable.LinkedHashSet[String] =
    inheritanceTraversal(typDeclFullName, subclassCache, inSuperDirection = false)

  /** Recursively returns all the super-types of the given type declaration. Does account for circular hierarchies.
    */
  private def allSuperClasses(typDeclFullName: String): mutable.LinkedHashSet[String] =
    inheritanceTraversal(typDeclFullName, superclassCache, inSuperDirection = true)

  private def inheritanceTraversal(
    typDeclFullName: String,
    cache: mutable.Map[String, mutable.LinkedHashSet[String]],
    inSuperDirection: Boolean
  ): mutable.LinkedHashSet[String] = {
    cache.get(typDeclFullName) match {
      case Some(superClasses) => superClasses
      case None =>
        val totalSuperclasses = (cpg.typeDecl
          .fullNameExact(typDeclFullName)
          .headOption match {
          case Some(curr) => inheritTraversal(curr, inSuperDirection)
          case None       => mutable.LinkedHashSet.empty
        }).map(_.fullName)
        cache.put(typDeclFullName, totalSuperclasses)
        totalSuperclasses
    }
  }

  private def inheritTraversal(
    cur: TypeDecl,
    inSuperDirection: Boolean,
    visitedNodes: mutable.LinkedHashSet[TypeDecl] = mutable.LinkedHashSet.empty
  ): mutable.LinkedHashSet[TypeDecl] = {
    if (visitedNodes.contains(cur)) return visitedNodes
    visitedNodes.addOne(cur)

    (if (inSuperDirection) cpg.typeDecl.fullNameExact(cur.fullName).flatMap(_.inheritsFromOut.referencedTypeDecl)
     else cpg.typ.fullNameExact(cur.fullName).flatMap(_.inheritsFromIn))
      .collectAll[TypeDecl]
      .to(mutable.LinkedHashSet) match {
      case classesToEval if classesToEval.isEmpty => visitedNodes
      case classesToEval =>
        classesToEval.flatMap(t => inheritTraversal(t, inSuperDirection, visitedNodes))
        visitedNodes
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
    def split(str: String, n: Int) = (str.take(n), str.drop(n + 1))
    val (fullName, signature)      = split(call.methodFullName, call.methodFullName.lastIndexOf(":"))
    val typeDeclFullName           = fullName.replace(s".${call.name}", "")
    val candidateInheritedMethods =
      cpg.typeDecl
        .fullNameExact(allSuperClasses(typeDeclFullName).toIndexedSeq*)
        .astChildren
        .isMethod
        .nameExact(call.name)
        .and(_.signatureExact(signature))
        .fullName
        .l
    if (candidateInheritedMethods.nonEmpty) {
      validM.put(
        call.methodFullName,
        validM.getOrElse(call.methodFullName, mutable.LinkedHashSet.empty) ++ mutable.LinkedHashSet.from(
          candidateInheritedMethods
        )
      )
      true
    } else {
      false
    }
  }

  private def linkDynamicCall(call: Call, dstGraph: DiffGraphBuilder): Unit = {
    // This call linker requires a method full name entry
    if (call.methodFullName.equals("<empty>") || call.methodFullName.equals(DynamicCallUnknownFullName)) return
    // Support for overriding
    resolveCallInSuperClasses(call)

    validM.get(call.methodFullName) match {
      case Some(tgts) =>
        val callsOut = call.callOut.fullName.toSetImmutable
        val tgtMs = tgts
          .flatMap(destMethod =>
            if (cpg.graph.indexManager.isIndexed(PropertyNames.FULL_NAME)) {
              methodFullNameToNode(destMethod)
            } else {
              cpg.method.fullNameExact(destMethod).headOption
            }
          )
          .toSet
        // Non-overridden methods linked as external stubs should be excluded if they are detected
        val (externalMs, internalMs) = tgtMs.partition(_.isExternal)
        (if (externalMs.nonEmpty && internalMs.nonEmpty) internalMs else tgtMs)
          .foreach { tgtM =>
            if (!callsOut.contains(tgtM.fullName)) {
              dstGraph.addEdge(call, tgtM, EdgeTypes.CALL)
            } else {
              fallbackToStaticResolution(call, dstGraph)
            }
          }
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

  private def nodesWithFullName(x: String): Iterable[NodeRef[? <: NodeDb]] =
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
