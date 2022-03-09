package io.joern.ghidra2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.{Languages, nodes}
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass, SimpleCpgPass}
import overflowdb.BatchedUpdate

class MetaDataPass(filename: String, cpg: Cpg) extends SimpleCpgPass(cpg) {

  override def run(diffGraph: BatchedUpdate.DiffGraphBuilder): Unit = {
    diffGraph.addNode(
      nodes
        .NewTypeDecl()
        .filename(filename)
        .fullName("<global>")
        .name("<global>")
    )

    diffGraph.addNode(
      nodes
        .NewMetaData()
        .language(Languages.GHIDRA)
        .version("0.1")
    )
  }

}
