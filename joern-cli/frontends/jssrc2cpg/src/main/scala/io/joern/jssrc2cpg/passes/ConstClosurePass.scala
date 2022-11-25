package io.joern.jssrc2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.CpgPass
import overflowdb.BatchedUpdate
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal._

/** A pass that identifies assignments of closures to constants and updates `METHOD` nodes accordingly.
  */
class ConstClosurePass(cpg: Cpg) extends CpgPass(cpg) {
  override def run(diffGraph: BatchedUpdate.DiffGraphBuilder): Unit = {
    for {
      assignment      <- cpg.assignment
      name            <- assignment.filter(_.code.startsWith("const ")).target.isIdentifier.name
      method          <- assignment.start.source.isMethodRef.referencedMethod
      enclosingMethod <- assignment.start.method.fullName
    } {
      diffGraph.setNodeProperty(method, "NAME", name)
      diffGraph.setNodeProperty(method, "FULL_NAME", s"$enclosingMethod:$name")
    }

  }
}
