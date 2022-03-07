package io.joern.ghidra2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.passes.ConcurrentWriterCpgPass

class NamespacePass(cpg: Cpg, filename: String) extends ConcurrentWriterCpgPass[Method](cpg) {

  override def runOnPart(diffGraph: DiffGraphBuilder, method: Method): Unit = {

    val namespaceNodeNode =
      nodes
        .NewNamespaceBlock()
        .filename(filename)
        .fullName(s"$filename:<global>")
        .name("<global>")
        .order(1)

    diffGraph.addNode(namespaceNodeNode)
  }
}
