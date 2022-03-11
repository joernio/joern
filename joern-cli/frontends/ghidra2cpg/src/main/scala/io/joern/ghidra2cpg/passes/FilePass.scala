package io.joern.ghidra2cpg.passes

import ghidra.program.flatapi.FlatProgramAPI
import ghidra.program.util.DefinedDataIterator
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewFile
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}
import io.shiftleft.passes.ConcurrentWriterCpgPass

import scala.jdk.CollectionConverters._
import scala.language.implicitConversions

class FilePass(cpg: Cpg, flatProgramAPI: FlatProgramAPI) extends ConcurrentWriterCpgPass[String](cpg) {

  override def generateParts(): Array[String] = { List(flatProgramAPI.getProgramFile.getAbsolutePath).toArray }

  override def runOnPart(diffGraph: DiffGraphBuilder, filePath: String): Unit = {
    val fileNode =
      NewFile()
        .name(filePath)
        .order(0)
    diffGraph.addNode(fileNode) // diffGraph.addEdge(srcNode, newFile, EdgeTypes.SOURCE_FILE)
    // val node = nodes
    //  .NewLiteral()
    //  .code(literal)
    //  .order(-1)
    //  .argumentIndex(-1)
    //  .typeFullName(literal)
    // diffGraph.addNode(node)
  }
}
