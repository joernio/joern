package io.joern.ghidra2cpg.passes.mips
import ghidra.program.model.listing.{Function, Program}
import ghidra.program.model.pcode.PcodeOp._
import io.joern.ghidra2cpg.Decompiler
import io.joern.ghidra2cpg.passes.FunctionPass
import io.joern.ghidra2cpg.processors.MipsProcessor
import io.joern.ghidra2cpg.utils.Nodes._
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewBlock
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}

import scala.jdk.CollectionConverters._
import scala.language.implicitConversions

class MipsFunctionPass(
  currentProgram: Program,
  address2Literal: Map[Long, String],
  filename: String,
  functions: List[Function],
  cpg: Cpg,
  decompiler: Decompiler
) extends FunctionPass(MipsProcessor, currentProgram, functions, cpg, decompiler) {

  override def runOnPart(diffGraphBuilder: DiffGraphBuilder, function: Function): Unit = {
    val localDiffGraph = new DiffGraphBuilder()
    // we need it just once with default settings
    val blockNode: NewBlock = nodes.NewBlock().code("").order(0)
    try {
      val methodNode =
        createMethodNode(decompiler, function, filename, checkIfExternal(currentProgram, function.getName))
      val methodReturn = createReturnNode()
      localDiffGraph.addNode(methodNode)
      localDiffGraph.addNode(blockNode)
      localDiffGraph.addEdge(methodNode, blockNode, EdgeTypes.AST)
      localDiffGraph.addNode(methodReturn)
      localDiffGraph.addEdge(methodNode, methodReturn, EdgeTypes.AST)
      handleParameters(localDiffGraph, function, methodNode)
      handleLocals(localDiffGraph, function, blockNode)
      handleBody(localDiffGraph, function, methodNode, blockNode)
    } catch {
      case exception: Exception =>
        exception.printStackTrace()
    }
    diffGraphBuilder.absorb(localDiffGraph)

  }
}
