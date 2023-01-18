package io.joern.jssrc2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.CpgPass
import overflowdb.BatchedUpdate

class ImportNodePass(cpg: Cpg) extends CpgPass(cpg) {
  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {}
}
