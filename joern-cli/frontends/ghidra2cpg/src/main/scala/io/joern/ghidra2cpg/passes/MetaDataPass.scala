package io.joern.ghidra2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.{Languages, nodes}
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}

class MetaDataPass(filename: String, cpg: Cpg, keyPool: IntervalKeyPool)
    extends ParallelCpgPass[String](cpg, keyPools = Some(keyPool.split(1))) {

  override def partIterator: Iterator[String] = List("").iterator

  override def runOnPart(part: String): Iterator[DiffGraph] = {
    implicit val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder

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

    Iterator(diffGraph.build())
  }
}
