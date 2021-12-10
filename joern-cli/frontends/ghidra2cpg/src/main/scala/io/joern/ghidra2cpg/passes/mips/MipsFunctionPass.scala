package io.joern.ghidra2cpg.passes.mips
import ghidra.program.model.address.GenericAddress
import ghidra.program.model.lang.Register
import ghidra.program.model.listing.{Function, Instruction, Program}
import ghidra.program.model.pcode.PcodeOp._
import ghidra.program.model.pcode.{PcodeOp, Varnode}
import ghidra.program.model.scalar.Scalar
import io.joern.ghidra2cpg.passes.FunctionPass
import io.joern.ghidra2cpg.processors.MipsProcessor
import io.joern.ghidra2cpg.utils.Nodes._
import io.joern.ghidra2cpg.{Decompiler, Types}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.CfgNodeNew
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool}
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters._
import scala.language.implicitConversions

class MipsFunctionPass(currentProgram: Program,
                       address2Literal: Map[Long, String],
                       filename: String,
                       function: Function,
                       cpg: Cpg,
                       keyPool: IntervalKeyPool,
                       decompiler: Decompiler)
    extends FunctionPass(new MipsProcessor, currentProgram, function, cpg, keyPool, decompiler) {
  private val logger = LoggerFactory.getLogger(classOf[MipsFunctionPass])

  def resolveVarNode(instruction: Instruction, input: Varnode, index: Int): CfgNodeNew = {
    if (input.isRegister) {
      var name = input.getHigh.getName
      val high = input.getHigh
      if (high != null && input.getDef != null && high.getName == "UNNAMED" && input.getDef != null && input.getDef.getInputs != null) {
        val symbol = input.getDef.getInputs.toList.lastOption
          .flatMap(x => Option(x.getHigh))
          .flatMap(x => Option(x.getSymbol))
        if (symbol.isDefined) {
          name = symbol.get.getName
        }
      }
      createIdentifier(name,
                       name,
                       index + 1,
                       Types.registerType(name),
                       instruction.getMinAddress.getOffsetAsBigInteger.intValue)
    } else if (input.isConstant) {
      createLiteral("0x" + input.getWordOffset.toHexString,
                    index + 1,
                    index + 1,
                    "0x" + input.getWordOffset.toHexString,
                    instruction.getMinAddress.getOffsetAsBigInteger.intValue)
    } else if (input.isUnique) {
      var valueString = ""
      if (input.getDescendants.asScala.toList.head.getOutput == null) {
        valueString = input.getDef.getInputs.toList.head.getAddress.getOffset.toString
      } else {
        valueString = input.getDescendants.asScala.toList.head.getOutput.getHigh.getName
      }

      val value = address2Literal.getOrElse(input.getDef.getInputs.toList.head.getAddress.getOffset, valueString)

      createLiteral(value,
                    index + 1,
                    index + 1,
                    input.getWordOffset.toHexString,
                    instruction.getMinAddress.getOffsetAsBigInteger.intValue)
    } else {
      // we default to literal
      // identifier could be useful too
      createLiteral(input.toString(),
                    index + 1,
                    index + 1,
                    input.toString(),
                    instruction.getMinAddress.getOffsetAsBigInteger.intValue)
    }
  }
  def handleAssignment(instruction: Instruction, callNode: CfgNodeNew, to: Varnode, index: Int): Unit = {
    val node = resolveVarNode(instruction, to, index)
    connectCallToArgument(callNode, node)
  }
  def handleTwoArguments(instruction: Instruction,
                         callNode: CfgNodeNew,
                         arg: Varnode,
                         arg1: Varnode,
                         operand: String,
                         name: String): Unit = {
    val firstOp = resolveVarNode(instruction, arg, 1)
    val secondOp = resolveVarNode(instruction, arg1, 2)
    val code = s"${firstOp.code} $operand ${secondOp.code}"
    val opNode = createCallNode(code = code, name, instruction.getMinAddress.getOffsetAsBigInteger.intValue)

    connectCallToArgument(opNode, firstOp)
    connectCallToArgument(opNode, secondOp)
    connectCallToArgument(callNode, opNode)
  }
  def handlePtrSub(instruction: Instruction, callNode: CfgNodeNew, varNode: Varnode, index: Int): Unit = {
    val arg = resolveVarNode(instruction, varNode, index)
    connectCallToArgument(callNode, arg)
  }
  def handleDefault(varNode: PcodeOp): Unit = {
    println("Unsupported " + varNode.toString + " " + varNode.getOpcode)
  }
  def resolveArgument(instruction: Instruction, callNode: CfgNodeNew, pcodeAst: PcodeOp, index: Int): Unit = {
    pcodeAst.getOpcode match {
      case INT_EQUAL | INT_NOTEQUAL | INT_SLESS | INT_SLESSEQUAL | INT_LESS | INT_LESSEQUAL =>
        logger.warn("INT_EQUAL | INT_NOTEQUAL | INT_SLESS | INT_SLESSEQUAL | INT_LESS | INT_LESSEQUAL ")
      case CALL | CALLIND =>
        handleAssignment(instruction, callNode, pcodeAst.getOutput, index)
      case INT_ADD | FLOAT_ADD =>
        handleTwoArguments(instruction,
                           callNode,
                           pcodeAst.getInput(0),
                           pcodeAst.getInput(1),
                           "+",
                           "<operator>.addition")
      case INT_DIV | FLOAT_DIV | INT_SDIV =>
        handleTwoArguments(instruction,
                           callNode,
                           pcodeAst.getInput(0),
                           pcodeAst.getInput(1),
                           "/",
                           "<operator>.division")
      case INT_SUB | FLOAT_SUB =>
        handleTwoArguments(instruction,
                           callNode,
                           pcodeAst.getInput(0),
                           pcodeAst.getInput(1),
                           "-",
                           "<operator>.subtraction")
      case INT_MULT | FLOAT_MULT =>
        handleTwoArguments(instruction,
                           callNode,
                           pcodeAst.getInput(0),
                           pcodeAst.getInput(1),
                           "*",
                           "<operator>.multiplication")
      case MULTIEQUAL | INDIRECT | PIECE => // not handled
      case INT_XOR =>
        handleTwoArguments(instruction, callNode, pcodeAst.getInput(0), pcodeAst.getInput(1), "^", "<operator>.xor")
      case INT_OR =>
        handleTwoArguments(instruction, callNode, pcodeAst.getInput(0), pcodeAst.getInput(1), "^", "<operator>.xor")
      case COPY | LOAD | STORE | SUBPIECE =>
        handleAssignment(instruction, callNode, pcodeAst.getOutput, index)
      case CAST =>
        // we need to "unpack" the def of the first input of the cast
        // eg. "(param_1 + 5)" in "(void *)(param_1 + 5)"
        resolveArgument(instruction, callNode, pcodeAst.getInput(0).getDef, index)
      case PTRSUB | PTRADD => handlePtrSub(instruction, callNode, pcodeAst.getOutput, index)
      case _               => //handleDefault(pcodeAst)

    }
  }

  def addCallArguments(instruction: Instruction, callNode: CfgNodeNew): Unit = {
    val opCodes = highFunction
      .getPcodeOps(instruction.getAddress())
      .asScala
      .toList
    if (opCodes.size < 2) {
      return
    }
    // first input is the address to the called function
    // we know it already
    val arguments = opCodes.head.getInputs.toList.drop(1)
    arguments.zipWithIndex.foreach {
      case (value, index) =>
        if (value.getDef != null)
          resolveArgument(instruction, callNode, value.getDef, index)
    }
  }

  def addInstructionArguments(instruction: Instruction, instructionNode: CfgNodeNew): Unit = {
    for (index <- 0 until instruction.getNumOperands) {
      val opObjects = instruction.getOpObjects(index)
      for (opObject <- opObjects) {
        opObject match {
          case register: Register =>
            val node = createIdentifier(register.getName,
                                        register.getName,
                                        index + 1,
                                        Types.registerType(register.getName),
                                        instruction.getMinAddress.getOffsetAsBigInteger.intValue)
            connectCallToArgument(instructionNode, node)
          case scalar: Scalar =>
            val node = createLiteral(scalar.toString(16, false, false, "", ""),
                                     index + 1,
                                     index + 1,
                                     scalar.toString(16, false, false, "", ""),
                                     instruction.getMinAddress.getOffsetAsBigInteger.intValue)
            connectCallToArgument(instructionNode, node)
          case genericAddress: GenericAddress =>
            val node = createLiteral(genericAddress.toString,
                                     index + 1,
                                     index + 1,
                                     genericAddress.toString,
                                     instruction.getMinAddress.getOffsetAsBigInteger.intValue)
            connectCallToArgument(instructionNode, node)
          case _ =>
            println(
              s"""Unsupported argument: $opObject ${opObject.getClass.getSimpleName}"""
            )
        }
      }
    }
  }

  // Iterating over operands and add edges to call
  override def handleArguments(
      instruction: Instruction,
      callNode: CfgNodeNew
  ): Unit = {
    if (instruction.getPcode.toList.isEmpty) {
      // nop && _nop
      return
    }
    // CALL is the last PcodeOp
    val opCodes = instruction.getPcode.toList //.last.getOpcode
    opCodes.last.getOpcode match {
      case CALLIND | CALL =>
        addCallArguments(instruction, callNode)
      case _ =>
        // regular instructions, eg. add/sub
        addInstructionArguments(instruction, callNode)
    }
  }

  override def runOnPart(part: String): Iterator[DiffGraph] = {
    try {
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
    } catch {
      case e: Exception =>
        e.printStackTrace()
        println(e.getMessage)
    }
    Iterator(diffGraph.build())
  }
}
