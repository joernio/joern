package io.joern.ghidra2cpg.passes.x86

import ghidra.program.model.listing.{Function, Program}
import io.joern.ghidra2cpg.Decompiler
import io.joern.ghidra2cpg.passes.FunctionPass
import io.joern.ghidra2cpg.processors.X86Processor
import io.joern.ghidra2cpg.utils.Nodes._
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}
import io.shiftleft.codepropertygraph.generated.nodes.{NewBlock, NewMethod}

import scala.language.implicitConversions

class X86FunctionPass(
  currentProgram: Program,
  fileName: String,
  functions: List[Function],
  cpg: Cpg,
  decompiler: Decompiler
) extends FunctionPass(new X86Processor, currentProgram, fileName, functions, cpg, decompiler) {

  override def handleBody(
    diffGraphBuilder: DiffGraphBuilder,
    function: Function,
    methodNode: NewMethod,
    blockNode: NewBlock
  ): Unit = {
    val instructions = getInstructions(function)
    if (instructions.nonEmpty) {
      var prevInstructionNode = addCallOrReturnNode(instructions.head)
      handleArguments(diffGraphBuilder, instructions.head, prevInstructionNode, function)
      diffGraphBuilder.addEdge(blockNode, prevInstructionNode, EdgeTypes.AST)
      diffGraphBuilder.addEdge(methodNode, prevInstructionNode, EdgeTypes.CFG)
      instructions.drop(1).foreach { instruction =>
        val instructionNode = addCallOrReturnNode(instruction)
        diffGraphBuilder.addNode(instructionNode)
        handleArguments(diffGraphBuilder, instruction, instructionNode, function)
        diffGraphBuilder.addEdge(blockNode, instructionNode, EdgeTypes.AST)
        // Not connecting the previous instruction,
        // if it is an unconditional jump
        // JMP is x86 specific
        if (!prevInstructionNode.code.startsWith("JMP")) {
          diffGraphBuilder.addEdge(prevInstructionNode, instructionNode, EdgeTypes.CFG)
        }
        prevInstructionNode = instructionNode
      }
    }
  }
}
