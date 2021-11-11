package io.joern.ghidra2cpg.passes.x86

import ghidra.app.decompiler.DecompInterface
import ghidra.program.model.listing.{Function, Program}
import io.joern.ghidra2cpg.passes.FunctionPass
import io.joern.ghidra2cpg.processors.X86Processor
import io.joern.ghidra2cpg.utils.Nodes._
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool}

import scala.language.implicitConversions
class X86FunctionPass(currentProgram: Program,
                      filename: String,
                      function: Function,
                      cpg: Cpg,
                      keyPool: IntervalKeyPool,
                      decompInterface: DecompInterface)
    extends FunctionPass(new X86Processor, currentProgram, filename, function, cpg, keyPool, decompInterface) {

  override def handleBody(): Unit = {
    if (instructions.nonEmpty) {
      var prevInstructionNode = addCallOrReturnNode(instructions.head)
      handleArguments(instructions.head, prevInstructionNode)
      diffGraph.addEdge(blockNode, prevInstructionNode, EdgeTypes.AST)
      diffGraph.addEdge(methodNode.get, prevInstructionNode, EdgeTypes.CFG)
      instructions.drop(1).foreach { instruction =>
        val instructionNode = addCallOrReturnNode(instruction)
        diffGraph.addNode(instructionNode)
        handleArguments(instruction, instructionNode)
        diffGraph.addEdge(blockNode, instructionNode, EdgeTypes.AST)
        // Not connecting previous instruction if it is an unconditional jump
        // JMP is x86 specific
        if (!prevInstructionNode.code.startsWith("JMP")) {
          diffGraph.addEdge(prevInstructionNode, instructionNode, EdgeTypes.CFG)
        }
        prevInstructionNode = instructionNode
      }
    }
  }
  override def runOnPart(part: String): Iterator[DiffGraph] = {
    methodNode = Some(createMethodNode(function, filename, checkIfExternal(currentProgram, function.getName)))
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
