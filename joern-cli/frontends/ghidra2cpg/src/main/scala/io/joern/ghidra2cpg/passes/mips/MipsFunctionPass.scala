package io.joern.ghidra2cpg.passes.mips
import ghidra.program.model.address.GenericAddress
import ghidra.program.model.lang.Register
import ghidra.program.model.listing.{Function, Instruction, Program}
import ghidra.program.model.pcode.PcodeOp._
import ghidra.program.model.pcode.{HighFunction, PcodeOp, PcodeOpAST, Varnode}
import ghidra.program.model.scalar.Scalar
import io.joern.ghidra2cpg.passes.FunctionPass
import io.joern.ghidra2cpg.processors.MipsProcessor
import io.joern.ghidra2cpg.utils.Utils._
import io.joern.ghidra2cpg.Types
import io.joern.ghidra2cpg.utils.Decompiler
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNodeNew, NewBlock}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}
import org.slf4j.LoggerFactory

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
  private val logger = LoggerFactory.getLogger(classOf[MipsFunctionPass])

  def resolveVarNode(instruction: Instruction, input: Varnode, index: Int): CfgNodeNew = {
    if (input.isRegister) {
      var name = input.getHigh.getName
      val high = input.getHigh
      if (
        high != null && input.getDef != null && high.getName == "UNNAMED" && input.getDef != null && input.getDef.getInputs != null
      ) {
        val symbol = input.getDef.getInputs.toList.lastOption
          .flatMap(x => Option(x.getHigh))
          .flatMap(x => Option(x.getSymbol))
        if (symbol.isDefined) {
          name = symbol.get.getName
        }
      }
      if (name == null) {
        name = input.getHigh.getSymbol.getName
      }
      createIdentifier(
        name,
        name,
        index + 1,
        Types.registerType(name),
        instruction.getMinAddress.getOffsetAsBigInteger.intValue
      )
    } else if (input.isConstant) {
      createLiteral(
        "0x" + input.getWordOffset.toHexString,
        index + 1,
        index + 1,
        "0x" + input.getWordOffset.toHexString,
        instruction.getMinAddress.getOffsetAsBigInteger.intValue
      )
    } else if (input.isUnique) {
      var valueString = ""
      if (input.getDescendants.asScala.toList.head.getOutput == null) {
        valueString = input.getDef.getInputs.toList.head.getAddress.getOffset.toString
      } else {
        valueString = input.getDescendants.asScala.toList.head.getOutput.getHigh.getName
      }

      val value = address2Literal.getOrElse(input.getDef.getInputs.toList.head.getAddress.getOffset, valueString)

      createLiteral(
        value,
        index + 1,
        index + 1,
        input.getWordOffset.toHexString,
        instruction.getMinAddress.getOffsetAsBigInteger.intValue
      )
    } else {
      // we default to literal
      // identifier could be useful too
      createLiteral(
        input.toString(),
        index + 1,
        index + 1,
        input.toString(),
        instruction.getMinAddress.getOffsetAsBigInteger.intValue
      )
    }
  }
  def handleAssignment(
    diffGraphBuilder: DiffGraphBuilder,
    instruction: Instruction,
    callNode: CfgNodeNew,
    to: Varnode,
    index: Int
  ): Unit = {
    val node = resolveVarNode(instruction, to, index)
    connectCallToArgument(diffGraphBuilder, callNode, node)
  }

  def handleTwoArguments(
    diffGraphBuilder: DiffGraphBuilder,
    instruction: Instruction,
    callNode: CfgNodeNew,
    pcodeOp: PcodeOp,
    operand: String,
    name: String
  ): Unit = {
    val firstOp  = resolveVarNode(instruction, pcodeOp.getInput(0), 1)
    val secondOp = resolveVarNode(instruction, pcodeOp.getInput(1), 2)
    val code     = s"${firstOp.code} $operand ${secondOp.code}"
    val opNode   = createCallNode(code = code, name, instruction.getMinAddress.getOffsetAsBigInteger.intValue)

    connectCallToArgument(diffGraphBuilder, opNode, firstOp)
    connectCallToArgument(diffGraphBuilder, opNode, secondOp)
    connectCallToArgument(diffGraphBuilder, callNode, opNode)
  }
  def handlePtrSub(
    diffGraphBuilder: DiffGraphBuilder,
    instruction: Instruction,
    callNode: CfgNodeNew,
    varNode: Varnode,
    index: Int
  ): Unit = {
    val arg = resolveVarNode(instruction, varNode, index)
    connectCallToArgument(diffGraphBuilder, callNode, arg)
  }
  def handleDefault(varNode: PcodeOp): Unit = {
    println("Unsupported " + varNode.toString + " " + varNode.getOpcode)
  }
  def resolveArgument(
    diffGraphBuilder: DiffGraphBuilder,
    instruction: Instruction,
    callNode: CfgNodeNew,
    pcodeAst: PcodeOp,
    index: Int
  ): Unit = {
    pcodeAst.getOpcode match {
      case INT_EQUAL | INT_NOTEQUAL | INT_SLESS | INT_SLESSEQUAL | INT_LESS | INT_LESSEQUAL =>
        logger.warn("INT_EQUAL | INT_NOTEQUAL | INT_SLESS | INT_SLESSEQUAL | INT_LESS | INT_LESSEQUAL ")
      case CALL | CALLIND =>
        handleAssignment(diffGraphBuilder, instruction, callNode, pcodeAst.getOutput, index)
      case INT_ADD | FLOAT_ADD =>
        handleTwoArguments(diffGraphBuilder, instruction, callNode, pcodeAst, "+", "<operator>.addition")
      case INT_DIV | FLOAT_DIV | INT_SDIV =>
        handleTwoArguments(diffGraphBuilder, instruction, callNode, pcodeAst, "/", "<operator>.division")
      case INT_SUB | FLOAT_SUB =>
        handleTwoArguments(diffGraphBuilder, instruction, callNode, pcodeAst, "-", "<operator>.subtraction")
      case INT_MULT | FLOAT_MULT =>
        handleTwoArguments(diffGraphBuilder, instruction, callNode, pcodeAst, "*", "<operator>.multiplication")
      case MULTIEQUAL | INDIRECT | PIECE => // not handled
      case INT_XOR =>
        handleTwoArguments(diffGraphBuilder, instruction, callNode, pcodeAst, "^", "<operator>.xor")
      case INT_OR =>
        handleTwoArguments(diffGraphBuilder, instruction, callNode, pcodeAst, "^", "<operator>.xor")
      case COPY | LOAD | STORE | SUBPIECE =>
        handleAssignment(diffGraphBuilder, instruction, callNode, pcodeAst.getOutput, index)
      case CAST =>
        // we need to "unpack" the def of the first input of the cast
        // eg. "(param_1 + 5)" in "(void *)(param_1 + 5)"
        if (pcodeAst.getInput(0).getDef != null) {
          resolveArgument(diffGraphBuilder, instruction, callNode, pcodeAst.getInput(0).getDef, index)
        }
      case PTRSUB | PTRADD => handlePtrSub(diffGraphBuilder, instruction, callNode, pcodeAst.getOutput, index)
      case _               => // handleDefault(pcodeAst)
    }
  }

  def addCallArguments(
    diffGraphBuilder: DiffGraphBuilder,
    instruction: Instruction,
    callNode: CfgNodeNew,
    highFunction: HighFunction
  ): Unit = {
    val opCodes: Seq[PcodeOpAST] = highFunction
      .getPcodeOps(instruction.getAddress())
      .asScala
      .toList
    if (opCodes.size < 2) {
      return
    }
    // first input is the address to the called function
    // we know it already
    val arguments = opCodes.head.getInputs.toList.drop(1)
    arguments.zipWithIndex.foreach { case (value, index) =>
      if (value.getDef != null)
        resolveArgument(diffGraphBuilder, instruction, callNode, value.getDef, index)
    }
  }

  def addInstructionArguments(
    diffGraphBuilder: DiffGraphBuilder,
    instruction: Instruction,
    instructionNode: CfgNodeNew
  ): Unit = {
    for (index <- 0 until instruction.getNumOperands) {
      val opObjects = instruction.getOpObjects(index)
      for (opObject <- opObjects) {
        opObject match {
          case register: Register =>
            val node = createIdentifier(
              register.getName,
              register.getName,
              index + 1,
              Types.registerType(register.getName),
              instruction.getMinAddress.getOffsetAsBigInteger.intValue
            )
            connectCallToArgument(diffGraphBuilder, instructionNode, node)
          case scalar: Scalar =>
            val node = createLiteral(
              scalar.toString(16, false, false, "", ""),
              index + 1,
              index + 1,
              scalar.toString(16, false, false, "", ""),
              instruction.getMinAddress.getOffsetAsBigInteger.intValue
            )
            connectCallToArgument(diffGraphBuilder, instructionNode, node)
          case genericAddress: GenericAddress =>
            val node = createLiteral(
              genericAddress.toString,
              index + 1,
              index + 1,
              genericAddress.toString,
              instruction.getMinAddress.getOffsetAsBigInteger.intValue
            )
            connectCallToArgument(diffGraphBuilder, instructionNode, node)
          case _ =>
            println(s"""Unsupported argument: $opObject ${opObject.getClass.getSimpleName}""")
        }
      }
    }
  }

  // Iterating over operands and add edges to call
  override def handleArguments(
    diffGraphBuilder: DiffGraphBuilder,
    instruction: Instruction,
    callNode: CfgNodeNew,
    function: Function
  ): Unit = {
    if (instruction.getPcode.toList.isEmpty) {
      // nop && _nop
      return
    }
    // CALL is the last PcodeOp
    val opCodes: Seq[PcodeOp] = instruction.getPcode.toList
    opCodes.last.getOpcode match {
      case CALLIND | CALL =>
        getHighFunction(function).foreach { highFunction =>
          addCallArguments(diffGraphBuilder, instruction, callNode, highFunction)
        }
      case _ =>
        // regular instructions, eg. add/sub
        addInstructionArguments(diffGraphBuilder, instruction, callNode)
    }
  }

  override def runOnPart(diffGraphBuilder: DiffGraphBuilder, function: Function): Unit = {
    val localDiffGraph = Cpg.newDiffGraphBuilder
    // we need it just once with default settings
    val blockNode: NewBlock = nodes.NewBlock().code("").order(0)
    val methodNode = createMethodNode(decompiler, function, filename, checkIfExternal(currentProgram, function.getName))
    val methodReturn = createReturnNode()
    localDiffGraph.addNode(methodNode)
    localDiffGraph.addNode(blockNode)
    localDiffGraph.addEdge(methodNode, blockNode, EdgeTypes.AST)
    localDiffGraph.addNode(methodReturn)
    localDiffGraph.addEdge(methodNode, methodReturn, EdgeTypes.AST)
    handleParameters(diffGraphBuilder, function, methodNode)
    handleLocals(diffGraphBuilder, function, blockNode)
    handleBody(diffGraphBuilder, function, methodNode, blockNode)
    diffGraphBuilder.absorb(localDiffGraph)
  }
}
