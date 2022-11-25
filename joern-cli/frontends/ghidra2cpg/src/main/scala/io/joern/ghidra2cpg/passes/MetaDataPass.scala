package io.joern.ghidra2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.{Languages, nodes}
import io.shiftleft.passes.CpgPass
import overflowdb.BatchedUpdate

class MetaDataPass(filename: String, cpg: Cpg) extends CpgPass(cpg) {

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
