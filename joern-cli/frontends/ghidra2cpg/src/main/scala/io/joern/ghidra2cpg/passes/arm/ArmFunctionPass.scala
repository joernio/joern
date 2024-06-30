package io.joern.ghidra2cpg.passes.arm

import ghidra.program.model.listing.{Function, Program}
import io.joern.ghidra2cpg.utils.Decompiler
import io.joern.ghidra2cpg.passes.FunctionPass
import io.joern.ghidra2cpg.processors.ArmProcessor
import io.joern.ghidra2cpg.utils.Utils.{checkIfExternal, createMethodNode, createReturnNode}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewBlock
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}

class ArmFunctionPass(
  currentProgram: Program,
  filename: String,
  functions: List[Function],
  cpg: Cpg,
  decompiler: Decompiler
) extends FunctionPass(ArmProcessor, currentProgram, functions, cpg, decompiler) {

  override def runOnPart(diffGraphBuilder: DiffGraphBuilder, function: Function): Unit = {
    val localDiffGraph = Cpg.newDiffGraphBuilder
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
