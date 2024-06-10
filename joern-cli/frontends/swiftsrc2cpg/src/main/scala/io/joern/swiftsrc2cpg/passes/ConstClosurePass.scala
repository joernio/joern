package io.joern.swiftsrc2cpg.passes

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, Method, MethodRef}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language._

/** A pass that identifies assignments of closures to constants and updates `METHOD` nodes accordingly.
  */
class ConstClosurePass(cpg: Cpg) extends CpgPass(cpg) {

  // Keeps track of how many times an identifier has been on the LHS of an assignment, by name
  private lazy val identifiersAssignedCount: Map[String, Int] =
    cpg.assignment.target.collectAll[Identifier].name.groupCount

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    handleConstClosures(diffGraph)
    handleClosuresDefinedAtExport(diffGraph)
    handleClosuresAssignedToMutableVar(diffGraph)
  }

  private def handleConstClosures(diffGraph: DiffGraphBuilder): Unit =
    for {
      assignment      <- cpg.assignment
      name            <- assignment.filter(_.code.startsWith("let ")).target.isIdentifier.name
      methodRef       <- assignment.start.source.isMethodRef
      method          <- methodRef.referencedMethod
      enclosingMethod <- assignment.start.method.fullName
    } {
      updateClosures(diffGraph, method, methodRef, enclosingMethod, name)
    }

  private def handleClosuresDefinedAtExport(diffGraph: DiffGraphBuilder): Unit =
    for {
      assignment <- cpg.assignment
      name <- assignment
        .filter(_.code.startsWith("@_exported"))
        .target
        .isCall
        .argument
        .isFieldIdentifier
        .canonicalName
        .l
      methodRef       <- assignment.start.source.ast.isMethodRef
      method          <- methodRef.referencedMethod
      enclosingMethod <- assignment.start.method.fullName
    } {
      updateClosures(diffGraph, method, methodRef, enclosingMethod, name)
    }

  private def handleClosuresAssignedToMutableVar(diffGraph: DiffGraphBuilder): Unit =
    // Handle closures assigned to mutable variables
    for {
      assignment      <- cpg.assignment
      name            <- assignment.start.code("^(var|let) .*").target.isIdentifier.name
      methodRef       <- assignment.start.source.ast.isMethodRef
      method          <- methodRef.referencedMethod
      enclosingMethod <- assignment.start.method.fullName
    } {
      // Conservatively update closures, i.e, if we only find 1 assignment where this variable is on the LHS
      if (identifiersAssignedCount.getOrElse(name, -1) == 1)
        updateClosures(diffGraph, method, methodRef, enclosingMethod, name)
    }

  private def updateClosures(
    diffGraph: DiffGraphBuilder,
    method: Method,
    methodRef: MethodRef,
    enclosingMethod: String,
    name: String
  ): Unit = {
    val fullName = s"$enclosingMethod:$name"
    diffGraph.setNodeProperty(methodRef, PropertyNames.METHOD_FULL_NAME, fullName)
    diffGraph.setNodeProperty(method, PropertyNames.NAME, name)
    diffGraph.setNodeProperty(method, PropertyNames.FULL_NAME, fullName)
  }
}
