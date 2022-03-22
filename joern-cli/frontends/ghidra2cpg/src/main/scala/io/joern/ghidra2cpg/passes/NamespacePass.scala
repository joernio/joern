package io.joern.ghidra2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.passes.ConcurrentWriterCpgPass

import java.io.File

class NamespacePass(cpg: Cpg, programFile: File) extends ConcurrentWriterCpgPass[String](cpg) {
  override def generateParts(): Array[String] = Array(programFile.getCanonicalFile.toString)

  override def runOnPart(diffGraph: DiffGraphBuilder, fileName: String): Unit = {
    val namespaceNodeNode =
      nodes
        .NewNamespaceBlock()
        .filename(fileName)
        .fullName(s"$fileName:<global>")
        .name("<global>")
        .order(1)

    diffGraph.addNode(namespaceNodeNode)
  }
}
