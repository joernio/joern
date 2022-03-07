package io.joern.ghidra2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.codepropertygraph.generated.{Languages, nodes}
import io.shiftleft.passes.ConcurrentWriterCpgPass

class MetaDataPass(cpg: Cpg, filename: String) extends ConcurrentWriterCpgPass[Method](cpg) {

  override def runOnPart(diffGraph: DiffGraphBuilder, method: Method): Unit = {

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
