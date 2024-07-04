package io.joern.ghidra2cpg.utils

import ghidra.app.util.template.TemplateSimplifier
import ghidra.program.model.listing.{CodeUnitFormat, CodeUnitFormatOptions, Function, Instruction}
import ghidra.program.model.pcode.PcodeOp.*
import ghidra.program.model.pcode.{HighFunction, PcodeOp, PcodeOpAST, Varnode}
import io.joern.ghidra2cpg.Types
//import io.joern.ghidra2cpg.utils.Utils.{createCallNode, createIdentifier, createLiteral}
import io.joern.ghidra2cpg.utils.Utils.*
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.CfgNodeNew
import org.slf4j.LoggerFactory
import io.shiftleft.codepropertygraph.generated.DiffGraphBuilder

import scala.jdk.CollectionConverters.*
import scala.language.implicitConversions
class State(argumentIndex: Int) {
  var argument: Int = argumentIndex
}

class PCodeMapper(
  diffGraphBuilder: DiffGraphBuilder,
  nativeInstruction: Instruction,
  functions: List[Function],
  highFunction: HighFunction,
  address2Literal: Map[Long, String]
) {
  private val logger = LoggerFactory.getLogger(getClass)
  private val pcodeOps: List[PcodeOp] =
    nativeInstruction.getPcode().toList

  val state = new State(argumentIndex = -1)

  val codeUnitFormat = new CodeUnitFormat(
    new CodeUnitFormatOptions(
      CodeUnitFormatOptions.ShowBlockName.NEVER,
      CodeUnitFormatOptions.ShowNamespace.NEVER,
      "",
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      new TemplateSimplifier()
    )
  )

  // Entry point
  def getNode: CfgNodeNew = {
    if (pcodeOps.isEmpty) {
      // It looks like that for some instructions,
      // like "bti c" getPcode() returns nothing
      logger.info(s"NO pcodes for $nativeInstruction")
      createCallNode(
        nativeInstruction.toString,
        nativeInstruction.toString,
        nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue()
      )
    } else {
      mapCallNode(pcodeOps.last)
    }
  }

  def createCall(name: String, code: String, index: Int = 1): CfgNodeNew = {
    createCallNode(code, name, nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue, index)
  }

  def handleSingleArgument(pcodeOp: PcodeOp, cpgOperationName: String, operation: String): CfgNodeNew = {
    val firstOp  = resolveVarNode(pcodeOp.getInput(0), 1)
    val callNode = createCall(cpgOperationName, nativeInstruction.toString) // s"$operation(${firstOp.code})")
    connectCallToArgument(callNode, firstOp)
    callNode
  }

  def handleTwoArguments(pcodeOp: PcodeOp, cpgOperationName: String, code: String, callIndex: Int = -1): CfgNodeNew = {
    val callNode =
      createCall(cpgOperationName, nativeInstruction.toString, callIndex)
    // we need this for MIPS
    if (pcodeOp.getOutput.isRegister) {
      val firstOp  = resolveVarNode(pcodeOp.getOutput, 1)
      val secondOp = resolveVarNode(pcodeOp.getInput(0), 2)
      val thirdOp  = resolveVarNode(pcodeOp.getInput(1), 3)
      connectCallToArgument(callNode, firstOp)
      connectCallToArgument(callNode, secondOp)
      connectCallToArgument(callNode, thirdOp)
    } else {
      val firstOp  = resolveVarNode(pcodeOp.getInput(0), 1)
      val secondOp = resolveVarNode(pcodeOp.getInput(1), 2)
      connectCallToArgument(callNode, firstOp)
      connectCallToArgument(callNode, secondOp)
    }
    callNode
  }

  def handleStore(pcodeOp: PcodeOp): CfgNodeNew = {
    val firstOp  = resolveVarNode(pcodeOp.getInput(1), 1)
    val secondOp = resolveVarNode(pcodeOp.getInput(2), 2)
    val callNode = createCall("<operator>.assignment", nativeInstruction.toString)
    connectCallToArgument(callNode, firstOp)
    connectCallToArgument(callNode, secondOp)
    callNode
  }
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
        index,
        Types.registerType(name),
        instruction.getMinAddress.getOffsetAsBigInteger.intValue
      )
    } else if (input.isConstant) {
      createLiteral(
        "0x" + input.getWordOffset.toHexString,
        index,
        index,
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
        index,
        index,
        input.getWordOffset.toHexString,
        instruction.getMinAddress.getOffsetAsBigInteger.intValue
      )
    } else {
      // we default to literal
      // identifier could be useful too
      createLiteral(
        input.toString(),
        index,
        index,
        input.toString(),
        instruction.getMinAddress.getOffsetAsBigInteger.intValue
      )
    }
  }
  def handleAssignment(pcodeOp: PcodeOp, code: String): CfgNodeNew = {
    val secondOp = resolveVarNode(pcodeOp.getInputs.headOption.get, 2)
    val callNode = createCall("<operator>.assignment", code = code) // s"${firstOp.code} = ${secondOp.code}")
    connectCallToArgument(callNode, secondOp)
    if (pcodeOp.getOutput.isRegister) {
      val firstOp = resolveVarNode(pcodeOp.getOutput, 1)
      connectCallToArgument(callNode, firstOp)
    } else {
      val firstOp = createIdentifier(
        pcodeOp.getOutput.toString(),
        pcodeOp.getOutput.toString,
        1,
        "TODO",
        pcodeOp.getSeqnum.getTarget.getOffsetAsBigInteger.intValue()
      )
      connectCallToArgument(callNode, firstOp)
    }
    callNode
  }
  def handleTwoArguments(
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

    connectCallToArgument(opNode, firstOp)
    connectCallToArgument(opNode, secondOp)
    connectCallToArgument(callNode, opNode)
  }
  def handlePtrSub(instruction: Instruction, callNode: CfgNodeNew, varNode: Varnode, index: Int): Unit = {
    val arg = resolveVarNode(instruction, varNode, index)
    connectCallToArgument(callNode, arg)
  }
  def handleAssignment(instruction: Instruction, callNode: CfgNodeNew, to: Varnode, index: Int): Unit = {
    val node = resolveVarNode(instruction, to, index)
    connectCallToArgument(callNode, node)
  }
  def resolveArgument(instruction: Instruction, callNode: CfgNodeNew, pcodeAst: PcodeOp, index: Int): Unit = {
    pcodeAst.getOpcode match {
      case INT_EQUAL | INT_NOTEQUAL | INT_SLESS | INT_SLESSEQUAL | INT_LESS | INT_LESSEQUAL =>
        logger.warn("INT_EQUAL | INT_NOTEQUAL | INT_SLESS | INT_SLESSEQUAL | INT_LESS | INT_LESSEQUAL ")
      case CALL | CALLIND =>
        handleAssignment(instruction, callNode, pcodeAst.getOutput, index)
      case INT_ADD | FLOAT_ADD =>
        handleTwoArguments(instruction, callNode, pcodeAst, "+", "<operator>.addition")
      case INT_DIV | FLOAT_DIV | INT_SDIV =>
        handleTwoArguments(instruction, callNode, pcodeAst, "/", "<operator>.division")
      case INT_SUB | FLOAT_SUB =>
        handleTwoArguments(instruction, callNode, pcodeAst, "-", "<operator>.subtraction")
      case INT_MULT | FLOAT_MULT =>
        handleTwoArguments(instruction, callNode, pcodeAst, "*", "<operator>.multiplication")
      case MULTIEQUAL | INDIRECT | PIECE => // not handled
      case INT_XOR =>
        handleTwoArguments(instruction, callNode, pcodeAst, "^", "<operator>.xor")
      case INT_OR =>
        handleTwoArguments(instruction, callNode, pcodeAst, "^", "<operator>.xor")
      case COPY | LOAD | STORE | SUBPIECE =>
        handleAssignment(instruction, callNode, pcodeAst.getOutput, index)
      case CAST =>
        // we need to "unpack" the def of the first input of the cast
        // eg. "(param_1 + 5)" in "(void *)(param_1 + 5)"
        if (pcodeAst.getInput(0).getDef != null) {
          resolveArgument(instruction, callNode, pcodeAst.getInput(0).getDef, index)
        }
      case PTRSUB | PTRADD => handlePtrSub(instruction, callNode, pcodeAst.getOutput, index)
      case _               => // handleDefault(pcodeAst)
    }
  }

  private def mapCallNode(pcodeOp: PcodeOp, index: Int = -1): CfgNodeNew = {
    // var callNode: CfgNodeNew = createCallNode("UNKNOWN", "UNKNOWN", -1)
    val callNode = pcodeOp.getOpcode match {
      // TODO add more pcode ops like CALL.*
      case BRANCH | BRANCHIND | CBRANCH =>
        val destination = resolveVarNode(pcodeOp.getInputs.head, 1)
        val callNode = createCallNode(
          nativeInstruction.toString,
          "<operator>.goto",
          nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue
        )
        connectCallToArgument(callNode, destination)
        callNode
      case RETURN =>
        createCall("ret", "ret")
      case CALL | CALLOTHER | CALLIND =>
        val calledFunction = codeUnitFormat
          .getOperandRepresentationString(nativeInstruction, 0)
          .split(">")
          .last
          .replace("[", "")
          .replace("]", "")
        val _callNode =
          createCallNode(calledFunction, calledFunction, nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue)
        val opCodes: Seq[PcodeOpAST] = highFunction
          .getPcodeOps(nativeInstruction.getAddress())
          .asScala
          .toList
        if (opCodes.size < 1) {
          return _callNode
        }
        // first input is the address to the called function
        // we know it already
        opCodes.head.getInputs.toList
          .drop(1)
          .zipWithIndex
          .foreach { case (value, index) =>
            if (value.getDef != null)
              resolveArgument(nativeInstruction, _callNode, value.getDef, index)
            else {
              // could/should be a constant
              val literalNode = createLiteral(
                "0x" + value.getWordOffset.toHexString,
                index + 1,
                index + 1,
                "0x" + value.getWordOffset.toHexString,
                nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue
              )
              connectCallToArgument(_callNode, literalNode)
            }
          }
        _callNode
      case BOOL_AND =>
        handleTwoArguments(pcodeOp, "<operator>.and", "&&")
      case BOOL_NEGATE =>
        handleSingleArgument(pcodeOp, "<operator>.negate", pcodeOp.getMnemonic)
      case BOOL_OR =>
        handleTwoArguments(pcodeOp, "<operator>.or", "||")
      case BOOL_XOR =>
        handleTwoArguments(pcodeOp, "<operator>.xor", "^^")

      case CAST =>
        handleSingleArgument(pcodeOp, "<operator>.cast", pcodeOp.getMnemonic)
      case CPOOLREF =>
        handleSingleArgument(pcodeOp, "<operator>.cpoolref", pcodeOp.getMnemonic)
      case EXTRACT =>
        handleSingleArgument(pcodeOp, "<operator>.extract", pcodeOp.getMnemonic)
      case FLOAT_ABS =>
        handleSingleArgument(pcodeOp, "<operator>.abs", pcodeOp.getMnemonic)
      case FLOAT_CEIL =>
        handleSingleArgument(pcodeOp, "<operator>.ceil", pcodeOp.getMnemonic)
      case FLOAT_FLOAT2FLOAT =>
        handleSingleArgument(pcodeOp, "<operator>.float2float", pcodeOp.getMnemonic)
      case FLOAT_FLOOR =>
        handleSingleArgument(pcodeOp, "<operator>.floor", pcodeOp.getMnemonic)
      case FLOAT_INT2FLOAT =>
        handleSingleArgument(pcodeOp, "<operator>.int2float", pcodeOp.getMnemonic)
      case FLOAT_LESS | INT_SLESS | INT_LESS =>
        handleTwoArguments(pcodeOp, "<operator>.goto", "<")
      case FLOAT_LESSEQUAL | INT_SLESSEQUAL | INT_LESSEQUAL =>
        handleTwoArguments(pcodeOp, "<operator>.lessThanEqual", "<=")
      case FLOAT_NAN =>
        handleSingleArgument(pcodeOp, "<operator>.nan", pcodeOp.getMnemonic)
      case FLOAT_ROUND =>
        handleSingleArgument(pcodeOp, "<operator>.round", pcodeOp.getMnemonic)
      case FLOAT_SQRT =>
        handleSingleArgument(pcodeOp, "<operator>.sqrt", pcodeOp.getMnemonic)
      case FLOAT_TRUNC =>
        handleSingleArgument(pcodeOp, "<operator>.trunc", pcodeOp.getMnemonic)
      case INSERT =>
        handleSingleArgument(pcodeOp, "<operator>.insert", pcodeOp.getMnemonic)
      case INT_2COMP =>
        handleSingleArgument(pcodeOp, "<operator>.int2comp", pcodeOp.getMnemonic)
      case INT_ADD | FLOAT_ADD | PTRADD =>
        handleTwoArguments(pcodeOp, "<operator>.addition", "+", index)
      case INT_AND =>
        handleTwoArguments(pcodeOp, "<operator>.and", "TODO: AND")
      case INT_CARRY =>
        handleTwoArguments(pcodeOp, "<operator>.TODO", "TODO: INT_CARRY")
      case INT_DIV | FLOAT_DIV | INT_SDIV =>
        handleTwoArguments(pcodeOp, "<operator>.division", "/")
      case FLOAT_EQUAL | INT_EQUAL =>
        handleTwoArguments(pcodeOp, "<operator>.equal", "==")
      case INT_NOTEQUAL | FLOAT_NOTEQUAL =>
        handleTwoArguments(pcodeOp, "<operator>.notEqual", "!=")
      case INT_LEFT =>
        handleTwoArguments(pcodeOp, "<operator>.shiftLeft", "<<")
      case INT_MULT | FLOAT_MULT =>
        handleTwoArguments(pcodeOp, "<operator>.multiplication", "*")
      case FLOAT_NEG | INT_NEGATE =>
        handleSingleArgument(pcodeOp, "<operator>.negation", pcodeOp.getMnemonic)
      case INT_OR =>
        handleTwoArguments(pcodeOp, "<operator>.or", "||")
      case INT_REM | INT_SREM =>
        handleTwoArguments(pcodeOp, "<operator>.modolo", "%")
      case INT_RIGHT | INT_SRIGHT =>
        handleTwoArguments(pcodeOp, "<operator>.shiftRight", ">>")
      case INT_SBORROW =>
        handleSingleArgument(pcodeOp, "<operator>.sborrow", pcodeOp.getMnemonic)
      case INT_SCARRY =>
        handleSingleArgument(pcodeOp, "<operator>.scarry", pcodeOp.getMnemonic)
      case INT_SEXT | INT_ZEXT =>
        handleSingleArgument(pcodeOp, "<operator>.extend", pcodeOp.getMnemonic)
      case INT_SUB | FLOAT_SUB | PTRSUB =>
        handleTwoArguments(pcodeOp, "<operator>.subtraction", "-")
      case INT_XOR =>
        handleTwoArguments(pcodeOp, "<operator>.xor", "^")
      case COPY | LOAD | SUBPIECE =>
        handleAssignment(pcodeOp, nativeInstruction.toString)
      case STORE =>
        handleStore(pcodeOp)
      case NEW =>
        handleSingleArgument(pcodeOp, "<operator>.new", pcodeOp.getMnemonic)
      case UNIMPLEMENTED | SEGMENTOP | MULTIEQUAL | INDIRECT | PIECE | PCODE_MAX =>
        createCall(
          "TODO: UNIMPLEMENTED | SEGMENTOP | MULTIEQUAL | INDIRECT | PIECE | PCODE_MAX | POPCOUNT ",
          "TODO: UNIMPLEMENTED | SEGMENTOP | MULTIEQUAL | INDIRECT | PIECE | PCODE_MAX | POPCOUNT "
        )
      case POPCOUNT =>
        handleAssignment(pcodeOp, nativeInstruction.toString)
      case _ =>
        createCall("NOT HANDLED", "NOT HANDLED")
    }
    callNode
  }

  def resolveVarNode(input: Varnode, index: Int): CfgNodeNew = {
    if (input.isRegister) {
      // we only care about the name
      createIdentifier(
        nativeInstruction.getProgram.getRegister(input).getName,
        nativeInstruction.getProgram.getRegister(input).getName,
        index,
        Types.registerType(nativeInstruction.getProgram.getRegister(input).getName),
        input.getAddress.getOffsetAsBigInteger.intValue
      )
    } else if (input.isUnique) {
      // unique could point to a string
      if (address2Literal.contains(input.getOffset)) {
        val value = address2Literal(input.getDef.getInputs.toList.head.getAddress.getOffset)
        createLiteral(
          value,
          index,
          index,
          input.getWordOffset.toHexString,
          nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue
        )
      } else {
        val uniques = pcodeOps
          // If the argument is a unique,
          // we try to resolve it
          .filter(x => x.getOutput == input)
          // Sometimes the first parameter is equal to the return value
          // filtering those out for now
          .filterNot(x => x.getInput(0) == input)
        mapCallNode(uniques.last, index)
      }
    } else {
      // input.isConstant || input.isAddress || input.isUnique
      createLiteral(
        "0x" + input.getWordOffset.toHexString,
        index,
        index,
        Types.registerType(input.getWordOffset.toHexString),
        -1
      )
    }
  }

  def connectCallToArgument(call: CfgNodeNew, argument: CfgNodeNew): Unit = {
    diffGraphBuilder.addNode(argument)
    diffGraphBuilder.addEdge(call, argument, EdgeTypes.ARGUMENT)
    diffGraphBuilder.addEdge(call, argument, EdgeTypes.AST)
  }
}
