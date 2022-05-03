package io.joern.ghidra2cpg.utils

import ghidra.program.model.listing.{CodeUnitFormat, CodeUnitFormatOptions, Function, Instruction}
import ghidra.program.model.pcode.PcodeOp._
import ghidra.program.model.pcode.{PcodeOp, Varnode}
import io.joern.ghidra2cpg.Types
import io.joern.ghidra2cpg.utils.Utils.{createCallNode, createIdentifier, createLiteral}
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNodeNew, NewCall}
import overflowdb.BatchedUpdate.DiffGraphBuilder

import scala.collection.mutable

/*
  TODO: resolve the unique arguments of the pcodeOps

  steps:
    1. Forward resolve all pcodes
    2. replace all uniques with previous calls
    3. last pcode is the actual instruction => create callnode
    4. add previous calls to the last callnode
 */
class PCodeMapper(diffGraphBuilder: DiffGraphBuilder, nativeInstruction: Instruction, functions: List[Function]) {
  var resolvedPcodeInstructions: mutable.HashMap[String, NewCall] = new mutable.HashMap[String, NewCall]()
  private val pcodeOps: Array[PcodeOp] = nativeInstruction.getPcode()
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
      true
    )
  )

  def getInstruction: Instruction = nativeInstruction

  def getPcodeOps: Array[PcodeOp] = pcodeOps

  def getOpcode: Int = pcodeOps.lastOption.get.getOpcode
  def getCallNode: CfgNodeNew = {
    pcodeOps.foreach(mapCallNode)
    createCallNode("TODO","TODO", -1)
  }
  def createCall(name: String): CfgNodeNew = {
    createCallNode(nativeInstruction.toString, name, nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue)
  }
  def handleTwoArguments(
    diffGraphBuilder: DiffGraphBuilder,
    instruction: Instruction,
    callNode: CfgNodeNew,
    pcodeOp: PcodeOp,
    //operand: String,
    name: String
  ): Unit = {
    val firstOp  = resolveVarNode(diffGraphBuilder, pcodeOp.getInput(0), callNode, 1)
    val secondOp = resolveVarNode(diffGraphBuilder, pcodeOp.getInput(1), callNode, 2)
    //val code     = s"${firstOp.code} $operand ${secondOp.code}"
    val opNode   = createCallNode(nativeInstruction.toString, name, instruction.getMinAddress.getOffsetAsBigInteger.intValue)

    connectCallToArgument(diffGraphBuilder, opNode, firstOp)
    connectCallToArgument(diffGraphBuilder, opNode, secondOp)
    connectCallToArgument(diffGraphBuilder, callNode, opNode)
  }
  def handleSingleAssignment(
    diffGraphBuilder: DiffGraphBuilder,
    instruction: Instruction,
    callNode: CfgNodeNew,
    varNode: Varnode,
    index: Int
  ): Unit = {
    val arg = resolveVarNode(diffGraphBuilder, varNode,callNode, index)
    connectCallToArgument(diffGraphBuilder, callNode, arg)
  }

  private def mapCallNode(pcodeOp: PcodeOp): CfgNodeNew = {
    var callNode:CfgNodeNew = createCallNode("UNKNOWN", "UNKNOWN", -1)
    val callCode = pcodeOp.getOpcode match {
      case BOOL_AND => "TODO"
      case BOOL_NEGATE => "TODO"
      case BOOL_OR =>
        callNode = createCall("<operator>.or")
        handleTwoArguments(diffGraphBuilder, nativeInstruction, callNode, pcodeOp, "||")
      case BOOL_XOR => 
        callNode = createCall("<operator>.xor")
        handleTwoArguments(diffGraphBuilder, nativeInstruction, callNode, pcodeOp, "^^")
      case BRANCH | BRANCHIND | CBRANCH => "<operator>.goto"
      case CALL | CALLOTHER | CALLIND => codeUnitFormat.getOperandRepresentationString(nativeInstruction, 0).split(">").last.replace("[", "").replace("]", "")
      case CAST => "TODO CAST"
      case COPY => "<operator>.assignment"
      case CPOOLREF => "TODO"
      case EXTRACT => "TODO"
      case FLOAT_ABS => "TODO"
      case FLOAT_CEIL => "TODO"
      case FLOAT_EQUAL | INT_EQUAL => "EQUAL TODO"
      case FLOAT_FLOAT2FLOAT => "TODO"
      case FLOAT_FLOOR => "TODO"
      case FLOAT_INT2FLOAT => "TODO"
      case FLOAT_LESS | INT_SLESS | INT_LESS => "LESS TODO"
      case FLOAT_LESSEQUAL => "TODO"
      case FLOAT_NAN => "TODO"
      case FLOAT_NEG => "TODO"
      case FLOAT_NOTEQUAL => "TODO"
      case FLOAT_ROUND => "TODO"
      case FLOAT_SQRT => "TODO"
      case FLOAT_TRUNC => "TODO"
      case INSERT => "TODO"
      case INT_2COMP => "TODO"
      case INT_ADD | FLOAT_ADD | PTRADD => 
        callNode = createCall("<operator>.addition")
        handleTwoArguments(diffGraphBuilder, nativeInstruction, callNode, pcodeOp, "+")
      case INT_AND => 
        callNode = createCall("<operator>.and")
        handleTwoArguments(diffGraphBuilder, nativeInstruction, callNode, pcodeOp, "/")
      case INT_CARRY => "TODO"
      case INT_DIV | FLOAT_DIV | INT_SDIV =>
        callNode = createCall("<operator>.division")
        handleTwoArguments(diffGraphBuilder, nativeInstruction, callNode, pcodeOp, "/")
      case INT_EQUAL | INT_NOTEQUAL | INT_SLESSEQUAL | INT_LESSEQUAL => nativeInstruction.toString
      case INT_LEFT => 
        callNode = createCall("<operator>.shiftleft")
        handleTwoArguments(diffGraphBuilder, nativeInstruction, callNode, pcodeOp, "<<")
      case INT_MULT | FLOAT_MULT => "<operator>.multiplication"
        callNode = createCall("<operator>.multiplication")
        handleTwoArguments(diffGraphBuilder, nativeInstruction, callNode, pcodeOp, "*")
      case INT_NEGATE => "TODO"
      case INT_OR => 
        callNode = createCall("<operator>.or")
        handleTwoArguments(diffGraphBuilder, nativeInstruction, callNode, pcodeOp, "*")
      case INT_REM | INT_SREM => "TODO"
      case INT_RIGHT => "TODO"
      case INT_SBORROW => "TODO"
      case INT_SCARRY => "TODO"
      case INT_SEXT => "TODO"
      case INT_SLESSEQUAL => "TODO"
      case INT_SRIGHT => "TODO"
      case INT_SUB | FLOAT_SUB | PTRSUB => "<operator>.subtraction"
      case INT_XOR => "<operator>.xor"
      case INT_ZEXT => "TODO"
      case LOAD => "<operator>.assignment"
      case MULTIEQUAL => "TODO"
      case MULTIEQUAL | INDIRECT | PIECE => "NOT HANDLED"
      case NEW => "TODO"
      case PCODE_MAX => "TODO"
      case POPCOUNT => "TODO"
      case RETURN => "RET" // TODO
      case SEGMENTOP => "TODO"
      case STORE => "<operator>.assignment"
      case SUBPIECE => "<operator>.assignment"
      case UNIMPLEMENTED => "UNIMPLEMENTED"
    }
    //val callNode = createCallNode(
    //  nativeInstruction.toString,
    //  callCode,
    //  nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue
    //)
    //if(pcodeOp.getOutput != null)
    //  resolvedPcodeInstructions += (pcodeOp.getOutput.getWordOffset.toString -> callNode)
    ////resolveArgument(pcodeOps.lastOption.get.getIn orNull)
    //pcodeOp.getInputs.zipWithIndex.foreach { case (param, index) =>
    //  //println("PARAM " + param)
    //  resolveArguments(diffGraphBuilder, param, callNode, index)
    //}
    callNode
  }

  def resolveVarNode(diffGraphBuilder: DiffGraphBuilder, input: Varnode, callNode: CfgNodeNew, index: Int): CfgNodeNew = {
    if (input.isRegister) {
      // we only care about the name
      createIdentifier(
        nativeInstruction.getProgram.getRegister(input).getName,
        nativeInstruction.getProgram.getRegister(input).getName,
        index + 1,
        Types.registerType(nativeInstruction.getProgram.getRegister(input).getName),
        input.getAddress.getOffsetAsBigInteger.intValue
      )
      //connectCallToArgument(diffGraphBuilder, callNode, n)
    } else if (input.isConstant || input.isAddress || input.isUnique) {
      createLiteral("0x" + input.getWordOffset.toHexString, index + 1, index + 1, Types.registerType(input.getWordOffset.toHexString), -1)
      //connectCallToArgument(diffGraphBuilder, callNode, n)
    } else {
      createLiteral("0x" + input.getWordOffset.toHexString, index + 1, index + 1, Types.registerType(input.getWordOffset.toHexString), -1)
      //connectCallToArgument(diffGraphBuilder, callNode, n)
    }
  }
  private def resolveArguments(diffGraphBuilder: DiffGraphBuilder, input: Varnode, callNode: CfgNodeNew, index: Int): Unit = {
    if (input.isRegister) {
      // we only care about the name
      val n = createIdentifier(
        nativeInstruction.getProgram.getRegister(input).getName,
        nativeInstruction.getProgram.getRegister(input).getName,
        index + 1,
        Types.registerType(nativeInstruction.getProgram.getRegister(input).getName),
        input.getAddress.getOffsetAsBigInteger.intValue
      )
      connectCallToArgument(diffGraphBuilder, callNode, n)
    } else if (input.isUnique) {
      val n = resolvedPcodeInstructions(input.getWordOffset.toString)
      connectCallToArgument(diffGraphBuilder, callNode, n)
      //getCallNode(input.getDef)
      //pcodeOps.filter(x=> x.getOutput == input && !x.getInputs.contains(input)).foreach{x=>
      //  val n = getCallNode(x)
      //  connectCallToArgument(diffGraphBuilder,callNode,n)
      //}
    } else if (input.isConstant || input.isAddress) {
      val n = createLiteral("0x" + input.getWordOffset.toHexString, index + 1, index + 1, Types.registerType(input.getWordOffset.toHexString), -1)
      connectCallToArgument(diffGraphBuilder, callNode, n)
    } else {
      val n = createLiteral("0x" + input.getWordOffset.toHexString, index + 1, index + 1, Types.registerType(input.getWordOffset.toHexString), -1)
      connectCallToArgument(diffGraphBuilder, callNode, n)
    }
  }

  def connectCallToArgument(diffGraphBuilder: DiffGraphBuilder, call: CfgNodeNew, argument: CfgNodeNew): Unit = {
    diffGraphBuilder.addNode(argument)
    diffGraphBuilder.addEdge(call, argument, EdgeTypes.ARGUMENT)
    diffGraphBuilder.addEdge(call, argument, EdgeTypes.AST)
  }
}
//case CAST => "TODO CAST"
//  // println("TODO: CAST")
//  //createCallNode("UNKNOWN", "UNKNOWN", nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue)
//// we need to "unpack" the def of the first input of the cast
//// eg. "(param_1 + 5)" in "(void *)(param_1 + 5)"
// if (pcodeOp.getInput(0).getDef != null)
//  val node = createCallNode("UNKNOWN", "UNKNOWN", nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue)
// else
//  val node = createCallNode("UNKNOWN", "UNKNOWN", nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue)
//  //resolveArgument(diffGraphBuilder, nativeInstruction, opNode, pcodeOps.last.getInput(0).getDef, index)
