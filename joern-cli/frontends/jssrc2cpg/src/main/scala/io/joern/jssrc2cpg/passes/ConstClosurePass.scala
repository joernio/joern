package io.joern.jssrc2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.codepropertygraph.generated.nodes.{Method, MethodRef}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language._

/** A pass that identifies assignments of closures to constants and updates `METHOD` nodes accordingly.
  */
class ConstClosurePass(cpg: Cpg) extends CpgPass(cpg) {
  override def run(diffGraph: DiffGraphBuilder): Unit = {
    // Handle const closures
    for {
      assignment      <- cpg.assignment
      name            <- assignment.filter(_.code.startsWith("const ")).target.isIdentifier.name
      methodRef       <- assignment.start.source.isMethodRef
      method          <- methodRef.referencedMethod
      enclosingMethod <- assignment.start.method.fullName
    } {
      updateClosures(diffGraph, method, methodRef, enclosingMethod, name)
    }
    // Handle closures defined at exports
    for {
      assignment <- cpg.assignment
      name <- assignment.filter(_.code.startsWith("export")).target.isCall.argument.isFieldIdentifier.canonicalName.l
      methodRef       <- assignment.start.source.ast.isMethodRef
      method          <- methodRef.referencedMethod
      enclosingMethod <- assignment.start.method.fullName
    } {
      updateClosures(diffGraph, method, methodRef, enclosingMethod, name)
    }
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
