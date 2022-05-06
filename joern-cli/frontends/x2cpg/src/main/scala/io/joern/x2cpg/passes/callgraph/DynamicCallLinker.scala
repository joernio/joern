package io.joern.x2cpg.passes.callgraph

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Method, MethodParameterIn, TypeDecl}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes, Operators, PropertyNames}
import io.shiftleft.passes.SimpleCpgPass
import io.shiftleft.semanticcpg.language._
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.traversal.Traversal
import overflowdb.{NodeDb, NodeRef}

import scala.collection.mutable
import scala.jdk.CollectionConverters._

/** We compute the set of possible call-targets for each dynamic call, and add them as CALL edges to the graph, based on
  * call.methodFullName, method.name and method.signature, the inheritance hierarchy and the AST of typedecls and
  * methods.
  *
  * This pass intentionally ignores the vtable mechanism based on BINDING nodes but does check for an existing call edge
  * before adding one. It assumes non-circular inheritance, on pain of endless recursion / stack overflow.
  *
  * This pass will attempt to use intra-procedural points-to information to refine possible call targets and can be best
  * described as variable type analysis (VTA).
  *
  * Based on the algorithm by Jang, Dongseok & Tatlock, Zachary & Lerner, Sorin. (2014). SAFEDISPATCH: Securing C++
  * Virtual Calls from Memory Corruption Attacks. 10.14722/ndss.2014.23287.
  */
class DynamicCallLinker(cpg: Cpg) extends SimpleCpgPass(cpg) {

  import DynamicCallLinker._
  // Used to track potential method candidates for a given method fullname. Since our method full names contain the type
  // decl we don't need to specify an addition map to wrap this in. LinkedHashSets are used here to preserve order in
  // the best interest of reproducibility during debugging.
  private val validM = mutable.Map.empty[String, mutable.LinkedHashSet[String]]
  // Used for dynamic programming as subtree's don't need to be recalculated later
  private val subclassCache = mutable.Map.empty[String, mutable.LinkedHashSet[String]]
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
      return;
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

  /** Recursively returns all the sub-types of the given type declaration. Does not account for circular hierarchies.
    */
  def allSubclasses(typDeclFullName: String): mutable.LinkedHashSet[String] = {
    subclassCache.get(typDeclFullName) match {
      case Some(value) => value
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

  /** Attempts to link a dynamic call to potential runtime callees. Does not consider inherited methods if not present.
    */
  private def linkDynamicCall(call: Call, dstGraph: DiffGraphBuilder): Unit = {
    validM.get(call.methodFullName) match {
      case Some(tgts) =>
        val callsOut = call.callOut.fullName.toSetImmutable
        filterWithVariableTypeInformation(call, tgts.toSet).foreach { destMethod =>
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
      case None => fallbackToStaticResolution(call, dstGraph)
    }
  }

  /** Attempts to refine possible call targets using points-to information. Treats properties as nullable to avoid NPE.
    */
  private def filterWithVariableTypeInformation(call: Call, tgts: Set[String]): Set[String] = {
    // We can refine possible calls using the potential allocation site
    val receivers = call.receiver.l
    val isThisReceiver = Traversal(receivers).isIdentifier.refsTo.collect {
      case p: MethodParameterIn if p.index == 0 => p
    }.nonEmpty

    if (call.receiver.isEmpty) {
      // Unable to use receiver/points-to information, resort to CHA
      tgts
    } else if (isThisReceiver) {
      // Receiver is the object itself, simply check object type
      val thisType = receivers.collect { case x: Identifier => Option(x.typeFullName) }.flatten.toSet
      filterTargets(tgts, thisType)
    } else {
      // Receiver is an object that may have points-to information, attempt to use it
      val allocatedSuperTypes =
        receivers
          .flatMap(_.pointsToOut)
          .collect {
            case x: Call if x.methodFullName == Operators.alloc || x.methodFullName == Operators.arrayInitializer =>
              x.evalType.toSeq
          }
          .flatten
          .toSet
      filterTargets(tgts, allocatedSuperTypes)
    }
  }

  @inline
  private def filterTargets(tgts: Set[String], allowedSet: Set[String]): Set[String] = {
    if (allowedSet.isEmpty)
      tgts // if allowed set is empty then we likely failed to receive points-to information
    else {
      tgts.filter { methodFullName =>
        methodMap(methodFullName).definingTypeDecl.fullName.headOption match {
          case Some(tgtTypeDecl) => allowedSet.contains(tgtTypeDecl)
          case None              => false
        }
      }
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
