package io.joern.jssrc2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language._
import overflowdb.BatchedUpdate
import overflowdb.traversal._

/** A pass that identifies assignments of closures to constants and updates `METHOD` nodes accordingly.
  */
class ConstClosurePass(cpg: Cpg) extends CpgPass(cpg) {
  override def run(diffGraph: BatchedUpdate.DiffGraphBuilder): Unit = {
    for {
      assignment      <- cpg.assignment
      name            <- assignment.filter(_.code.startsWith("const ")).target.isIdentifier.name
      methodRef       <- assignment.start.source.isMethodRef
      method          <- methodRef.referencedMethod
      enclosingMethod <- assignment.start.method.fullName
    } {
      diffGraph.setNodeProperty(methodRef, PropertyNames.METHOD_FULL_NAME, s"$enclosingMethod:$name")
      diffGraph.setNodeProperty(method, PropertyNames.NAME, name)
      diffGraph.setNodeProperty(method, PropertyNames.FULL_NAME, s"$enclosingMethod:$name")
    }

  }
}
