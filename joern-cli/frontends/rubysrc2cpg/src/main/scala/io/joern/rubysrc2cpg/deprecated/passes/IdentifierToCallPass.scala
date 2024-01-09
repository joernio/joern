package io.joern.rubysrc2cpg.deprecated.passes

import io.joern.x2cpg.Defines as XDefines
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, Local, Method, NewCall}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes}
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.shiftleft.semanticcpg.language.*

/** Scans for identifiers that are meant to be calls but are simply invoked without parenthesis'. Uses heuristics here
  * and there to make the distinction.
  */
class IdentifierToCallPass(cpg: Cpg) extends ConcurrentWriterCpgPass[Method](cpg) {

  private val methodNameMap: Map[String, List[Method]] = cpg.method.groupBy(_.name)
  private val methodNameSet: Set[String]               = methodNameMap.keySet

  override def generateParts(): Array[Method] = cpg.method.toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, part: Method): Unit = {
    val identifiersToRemove = part.local
      .filter(sharesNameWithMethod)
      .where(local =>
        local.referencingIdentifiers
          .whereNot(isTargetOfAssignment)
      )
      .flatMap(local => convertToCall(diffGraph, local))
      .toSet

    identifiersToRemove.foreach { identifier =>
      // Remove identifiers
      diffGraph.removeNode(identifier)
    }
  }

  /** @return
    *   true if the call shares a name with a defined method, false if otherwise.
    */
  private def sharesNameWithMethod(node: Local): Boolean =
    methodNameSet.contains(node.name)

  /** Determines if the identifier is the LHS of an assignment.
    */
  private def isTargetOfAssignment(nodes: Iterator[Identifier]): Iterator[Identifier] =
    if (nodes.nonEmpty) nodes.where(_.inAssignment).argumentIndex(1)
    else nodes

  /** Removes the local node and replaces all if its referencing identifiers with call nodes.
    */
  private def convertToCall(diffGraph: DiffGraphBuilder, local: Local): Set[Identifier] = {

    local.referencingIdentifiers.foreach { identifier =>
      // Create call with not much type info - this will be handled in type propagation
      val newCall = NewCall()
        .name(identifier.name)
        .code(identifier.code)
        .typeFullName(Defines.Any)
        .dispatchType(DispatchTypes.DYNAMIC_DISPATCH)
        .methodFullName(XDefines.DynamicCallUnknownFullName)
        .argumentName(identifier.argumentName)
        .argumentIndex(identifier.argumentIndex)
        .lineNumber(identifier.lineNumber)
        .columnNumber(identifier.columnNumber)
        .order(identifier.order)
      // Persist node in-place
      diffGraph.addNode(newCall)
      identifier.outE.filterNot(_.label == EdgeTypes.REF).foreach(e => diffGraph.addEdge(newCall, e.inNode, e.label))
      identifier.inE.foreach(e => diffGraph.addEdge(e.outNode, newCall, e.label))
    }

    // Finally, remove local
    diffGraph.removeNode(local)

    local.referencingIdentifiers.toSet
  }

}
