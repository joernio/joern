package io.joern.jssrc2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.CpgPass
import overflowdb.BatchedUpdate

/** This pass creates `IMPORT` nodes by looking for calls to `require`. `IMPORT` nodes are linked to existing dependency
  * nodes, or, if no suitable dependency node exists, a dependency node is created.
  */
class ImportsPass(cpg: Cpg) extends CpgPass(cpg) {
  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {
    ???
  }
}
