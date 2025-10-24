package io.joern.x2cpg.frontendspecific.swiftsrc2cpg

import io.shiftleft.codepropertygraph.generated.nodes.{Method, MethodRef}
import io.shiftleft.codepropertygraph.generated.{Cpg, PropertyNames}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*

/** A pass that identifies assignments of closures to constants and updates `METHOD` nodes accordingly.
  */
class ConstClosurePass(cpg: Cpg) extends CpgPass(cpg) {

  // Keeps track of how many times an identifier has been on the LHS of an assignment, by name
  private lazy val identifiersAssignedCount: Map[String, Int] =
    cpg.assignment.target.isIdentifier.name.groupCount

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    handleConstClosures(diffGraph)
    handleClosuresDefinedAtExport(diffGraph)
    handleClosuresAssignedToMutableVar(diffGraph)
  }

  private def qualifies(methodRef: MethodRef): Boolean = {
    // We only want this pass to handle closures where the frontend itself
    // was not able to generate the correct calls, typedecls, and bindings itself.
    cpg.typeDecl.fullNameExact(methodRef.methodFullName).bindsOut.nameExact(Defines.ClosureApplyMethodName).isEmpty
  }

  private def handleConstClosures(diffGraph: DiffGraphBuilder): Unit =
    for {
      assignment      <- cpg.assignment
      name            <- assignment.start.code("^let .*").target.isIdentifier.name
      methodRef       <- assignment.start.source.isMethodRef if qualifies(methodRef)
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
      methodRef       <- assignment.start.source.ast.isMethodRef if qualifies(methodRef)
      method          <- methodRef.referencedMethod
      enclosingMethod <- assignment.start.method.fullName
    } {
      updateClosures(diffGraph, method, methodRef, enclosingMethod, name)
    }

  private def handleClosuresAssignedToMutableVar(diffGraph: DiffGraphBuilder): Unit =
    // Handle closures assigned to mutable variables
    for {
      assignment      <- cpg.assignment
      name            <- assignment.start.code("^var .*").target.isIdentifier.name
      methodRef       <- assignment.start.source.isMethodRef if qualifies(methodRef)
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
    diffGraph.setNodeProperty(methodRef, PropertyNames.MethodFullName, fullName)
    diffGraph.setNodeProperty(method, PropertyNames.Name, name)
    diffGraph.setNodeProperty(method, PropertyNames.FullName, fullName)
  }
}
