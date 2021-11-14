package io.joern.ghidra2cpg.passes.arm

import ghidra.program.model.listing.{Function, Program}
import io.joern.ghidra2cpg.Decompiler
import io.joern.ghidra2cpg.passes.FunctionPass
import io.joern.ghidra2cpg.processors.ArmProcessor
import io.joern.ghidra2cpg.utils.Nodes.{checkIfExternal, createMethodNode, createReturnNode}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool}

class ArmFunctionPass(currentProgram: Program,
                      filename: String,
                      function: Function,
                      cpg: Cpg,
                      keyPool: IntervalKeyPool,
                      decompiler: Decompiler)
    extends FunctionPass(new ArmProcessor, currentProgram, function, cpg, keyPool, decompiler) {

  override def runOnPart(part: String): Iterator[DiffGraph] = {
    methodNode = Some(
      createMethodNode(decompiler, function, filename, checkIfExternal(currentProgram, function.getName)))
    diffGraph.addNode(methodNode.get)
    diffGraph.addNode(blockNode)
    diffGraph.addEdge(methodNode.get, blockNode, EdgeTypes.AST)
    val methodReturn = createReturnNode()
    diffGraph.addNode(methodReturn)
    diffGraph.addEdge(methodNode.get, methodReturn, EdgeTypes.AST)
    handleParameters()
    handleLocals()
    handleBody()
    Iterator(diffGraph.build())
  }
}
