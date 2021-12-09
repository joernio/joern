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

  def resolveVarNode(instruction: Instruction, callNode: CfgNodeNew, input: Varnode, index: Int): CfgNodeNew = {
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
      val value = address2Literal.getOrElse(input.getDef.getInputs.toList.head.getAddress.getOffset,
                                            input.getDef.getInputs.toList.head.getAddress.getOffset.toString)
      createLiteral(value,
                    index + 1,
                    index + 1,
                    input.getWordOffset.toHexString,
                    instruction.getMinAddress.getOffsetAsBigInteger.intValue)
    } else {
      //if (input.getDef != null) {
      //  resolveArgument(instruction, callNode, input.getDef, index)
      //} else {
      //  input.toString()
      createLiteral(input.toString(),
                    index + 1,
                    index + 1,
                    input.toString(),
                    instruction.getMinAddress.getOffsetAsBigInteger.intValue)
      //}
    }
  }
  def handleAssignment(instruction: Instruction, callNode: CfgNodeNew, to: Varnode, from:Varnode, index: Int):Unit ={
    val fromNode = resolveVarNode(instruction, callNode, from, 1)
    val toNode = resolveVarNode(instruction, callNode, to, 2)
    //val assingmentNode = createCallNode(code = s"${toNode.code} = ${fromNode.code}", "<opeator>.assignment", instruction.getMinAddress.getOffsetAsBigInteger.intValue)

    //connectCallToArgument(assingmentNode, toNode)
    //connectCallToArgument(assingmentNode, fromNode)
    connectCallToArgument(callNode,toNode)
    connectCallToArgument(callNode,fromNode)
  }
  def handleTwoArguments(instruction: Instruction,
                         callNode: CfgNodeNew,
                         arg: Varnode,
                         arg1: Varnode,
                         operand: String,
                         name: String): Unit = {
    val firstOp = resolveVarNode(instruction, callNode, arg, 1)
    val secondOp = resolveVarNode(instruction, callNode, arg1, 2)
    val code = s"${firstOp.code} $operand ${secondOp.code}"
    val opNode = createCallNode(code = code, name, instruction.getMinAddress.getOffsetAsBigInteger.intValue)

    connectCallToArgument(opNode, firstOp)
    connectCallToArgument(opNode, secondOp)
    connectCallToArgument(callNode, opNode)
  }
  def handlePtrSub(instruction: Instruction, callNode: CfgNodeNew, varNode: Varnode, index: Int): Unit = {
    val arg = resolveVarNode(instruction, callNode, varNode, index)
    connectCallToArgument(callNode, arg)
  }
  def handleDefault(varNode: PcodeOp): Unit = {
    println("Unsupported " + varNode.toString + " " + varNode.getOpcode)
  }
  def resolveArgument(instruction: Instruction, callNode: CfgNodeNew, pcodeAst: PcodeOp, index: Int): Unit = {
    pcodeAst.getOpcode match {
      /*
    BRANCH = 4;
    CBRANCH = 5;
    BRANCHIND = 6;
    CALLIND = 8;
    CALLOTHER = 9;
    RETURN = 10;
    INT_ZEXT = 17;
    INT_SEXT = 18;
    INT_CARRY = 21;
    INT_SCARRY = 22;
    INT_SBORROW = 23;
    INT_2COMP = 24;
    INT_NEGATE = 25;
    INT_AND = 27;
    INT_OR = 28;
    INT_LEFT = 29;
    INT_RIGHT = 30;
    INT_SRIGHT = 31;
    INT_REM = 35;
    INT_SREM = 36;
    BOOL_NEGATE = 37;
    BOOL_XOR = 38;
    BOOL_AND = 39;
    BOOL_OR = 40;
    FLOAT_EQUAL = 41;
    FLOAT_NOTEQUAL = 42;
    FLOAT_LESS = 43;
    FLOAT_LESSEQUAL = 44;
    FLOAT_NAN = 46;
    FLOAT_NEG = 51;
    FLOAT_ABS = 52;
    FLOAT_SQRT = 53;
    FLOAT_INT2FLOAT = 54;
    FLOAT_FLOAT2FLOAT = 55;
    FLOAT_TRUNC = 56;
    FLOAT_CEIL = 57;
    FLOAT_FLOOR = 58;
    FLOAT_ROUND = 59;
    INDIRECT = 61;
    SEGMENTOP = 67;
    CPOOLREF = 68;
    NEW = 69;
    INSERT = 70;
    EXTRACT = 71;
    POPCOUNT = 72;
    PCODE_MAX = 73;
       */
      case INT_EQUAL | INT_NOTEQUAL | INT_SLESS | INT_SLESSEQUAL | INT_LESS | INT_LESSEQUAL => logger.warn("INT_EQUAL | INT_NOTEQUAL | INT_SLESS | INT_SLESSEQUAL | INT_LESS | INT_LESSEQUAL ")
      case CALL =>
        pcodeAst.getInputs.zipWithIndex.foreach {
        case (value, index) =>
          if (value.getDef != null)
            resolveArgument(instruction, callNode, value.getDef, index)
          else
            resolveVarNode(instruction, callNode, value, index)
      }
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
      case INT_XOR | BOOL_XOR =>
        handleTwoArguments(instruction, callNode, pcodeAst.getInput(0), pcodeAst.getInput(1), "^", "<operator>.xor")
      case INT_OR =>
        handleTwoArguments(instruction, callNode, pcodeAst.getInput(0), pcodeAst.getInput(1), "^", "<operator>.xor")
      case COPY | LOAD | STORE | SUBPIECE => handleAssignment(instruction, callNode, pcodeAst.getOutput, pcodeAst.getInput(0), index)
      case CAST =>
        // we need to "unpack" the def of the first input of the cast
        // eg. "(param_1 + 5)" in "(void *)(param_1 + 5)"
        resolveArgument(instruction, callNode, pcodeAst.getInput(0).getDef, index)
      case PTRSUB | PTRADD => handlePtrSub(instruction, callNode, pcodeAst.getOutput, index)
      case _      => handleDefault(pcodeAst)

    }
  }

  def resolveCallIndArguments(instruction: Instruction, callNode: CfgNodeNew): Unit = {
    val opCodes = highFunction
      .getPcodeOps(instruction.getAddress())
      .asScala
      .toList
    if (opCodes.size < 2) {
      return
    }
    val arguments = opCodes.head.getInputs.toList.drop(1)
    arguments.zipWithIndex.foreach {
      case (value, index) =>
        if (value.getDef != null)
          resolveArgument(instruction, callNode, value.getDef, index)
    }
  }
  def resolveCallArguments(instruction: Instruction, callNode: CfgNodeNew): Unit = {
    // we know that this is a call
    val opCodes = highFunction
      .getPcodeOps(instruction.getAddress())
      .asScala
      .toList
    if (opCodes.size < 2) {
      return
    }
    val arguments = opCodes.head.getInputs.toList.drop(1)
    arguments.zipWithIndex.foreach {
      case (value, index) =>
          resolveArgument(instruction, callNode, value.getDef, index)
    }
  }
  def addArguments(instruction: Instruction, instructionNode: CfgNodeNew): Unit = {
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
        resolveCallIndArguments(instruction, callNode)
      case _ =>
        // regular instructions, eg. add/sub
        addArguments(instruction, callNode)
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
