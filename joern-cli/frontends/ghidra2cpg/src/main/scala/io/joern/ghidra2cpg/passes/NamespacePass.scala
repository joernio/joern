package io.joern.ghidra2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}

class NamespacePass(cpg: Cpg, filename: String, keyPool: IntervalKeyPool)
    extends ParallelCpgPass[String](
      cpg,
      keyPools = Some(keyPool.split(1))
    ) {
  override def partIterator: Iterator[String] = List("").iterator

  override def runOnPart(part: String): Iterator[DiffGraph] = {
    implicit val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder

    val dummyNamespaceBlock = nodes
      .NewNamespaceBlock()
      .filename("<unknown>")
      .fullName("<global>")
      .name("<global>")
      .order(1)
    val namespaceNodeNode =
      nodes
        .NewNamespaceBlock()
        .filename(filename)
        .fullName(s"$filename:<global>")
        .name("<global>")
        .order(1)

    diffGraph.addNode(dummyNamespaceBlock)
    diffGraph.addNode(namespaceNodeNode)
    Iterator(diffGraph.build())
  }
}
