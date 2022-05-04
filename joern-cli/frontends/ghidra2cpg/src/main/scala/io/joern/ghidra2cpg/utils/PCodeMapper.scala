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

import scala.jdk.CollectionConverters._
import scala.language.implicitConversions
/*
  TODO: resolve the unique arguments of the pcodeOps

  steps:
    1. Forward resolve all pcodes
    2. replace all uniques with previous calls
    3. last pcode is the actual instruction => create callnode
    4. add previous calls to the last callnode
 */
class PCodeMapper(diffGraphBuilder: DiffGraphBuilder, nativeInstruction: Instruction, functions: List[Function], decompiler:Decompiler) {
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
  def handleSingleArgument(
                            diffGraphBuilder: DiffGraphBuilder,
                            callNode: CfgNodeNew,
                            pcodeOp: PcodeOp,
                          ): Unit = {

    val firstOp  = resolveVarNode(pcodeOp.getInput(0),  1)
    connectCallToArgument(diffGraphBuilder, callNode, firstOp)
  }
  def handleTwoArguments(
    diffGraphBuilder: DiffGraphBuilder,
    instruction: Instruction,
    callNode: CfgNodeNew,
    pcodeOp: PcodeOp,
    name: String
  ): Unit = {
    val firstOp  = resolveVarNode(pcodeOp.getInput(0), 1)
    val secondOp = resolveVarNode(pcodeOp.getInput(1), 2)
    val opNode   = createCallNode(nativeInstruction.toString, name, instruction.getMinAddress.getOffsetAsBigInteger.intValue)

    connectCallToArgument(diffGraphBuilder, opNode, firstOp)
    connectCallToArgument(diffGraphBuilder, opNode, secondOp)
    connectCallToArgument(diffGraphBuilder, callNode, opNode)
  }
  def handleAssignment(
    diffGraphBuilder: DiffGraphBuilder,
    callNode: CfgNodeNew,
    varNode: Varnode,
    index: Int
  ): Unit = {
    val arg = resolveVarNode(varNode, index)
    connectCallToArgument(diffGraphBuilder, callNode, arg)
  }

  private def mapCallNode(pcodeOp: PcodeOp): CfgNodeNew = {
    var callNode:CfgNodeNew = createCallNode("UNKNOWN", "UNKNOWN", -1)
    pcodeOp.getOpcode match {
      case BOOL_AND =>
        callNode = createCall("<operator>.and")
        handleTwoArguments(diffGraphBuilder, nativeInstruction, callNode, pcodeOp, "&&")
      case BOOL_NEGATE =>
        callNode = createCall("<operator>.negate")
        handleAssignment(diffGraphBuilder,  callNode, pcodeOp.getOutput, 0)
      case BOOL_OR =>
        callNode = createCall("<operator>.or")
        handleTwoArguments(diffGraphBuilder, nativeInstruction, callNode, pcodeOp, "||")
      case BOOL_XOR => 
        callNode = createCall("<operator>.xor")
        handleTwoArguments(diffGraphBuilder, nativeInstruction, callNode, pcodeOp, "^^")
      case BRANCH | BRANCHIND | CBRANCH =>
        callNode = createCall("<operator>.goto")
        handleSingleArgument(diffGraphBuilder, callNode, pcodeOp)
      case CALL | CALLOTHER | CALLIND =>
        val calledFunction = codeUnitFormat.getOperandRepresentationString(nativeInstruction, 0)
          .split(">")
          .last
          .replace("[", "")
          .replace("]", "")
        val callee = functions.filter(_.getName == calledFunction)
        if (callee.nonEmpty) {
          // Array of tuples containing (checked parameter name, parameter index, parameter data type)
          var checkedParameters = Array.empty[(String, Int, String)]

          if (callee.head.isThunk) {
            // thunk functions contain parameters already
            val parameters = callee.head.getParameters
            // TODO:
            checkedParameters = parameters.map { parameter =>
              val checkedParameter =
                if (parameter.getRegister == null) parameter.getName
                else parameter.getRegister.getName

              // checked parameter name, parameter index, parameter data type
              (checkedParameter, parameter.getOrdinal + 1, parameter.getDataType.getName)
            }
          } else {
            // non thunk functions do not contain function parameters by default
            // need to decompile function to get parameter information
            // decompilation for a function is cached so subsequent calls to decompile should be free
            // TODO: replace this later on
            val parameters = decompiler
              .toHighFunction(callee.head)
              .get
              .getLocalSymbolMap
              .getSymbols
              .asScala
              .toSeq
              .filter(_.isParameter)
              .toArray
            checkedParameters = parameters.map { parameter =>
              val checkedParameter =
                if (parameter.getStorage.getRegister == null) parameter.getName
                else parameter.getStorage.getRegister.getName

              // checked parameter name, parameter index, parameter data type
              (checkedParameter, parameter.getCategoryIndex + 1, parameter.getDataType.getName)
            }
          }
          checkedParameters.foreach { case (checkedParameter, index, dataType) =>
            val node = createIdentifier(
              checkedParameter,
              checkedParameter,
              index,
              Types.registerType(dataType),
              nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue
            )
            connectCallToArgument(diffGraphBuilder, callNode, node)
          }
        }

        println("BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB "+calledFunction)
        callNode = createCall(calledFunction)
      case CAST => "TODO CAST"
      case CPOOLREF => "TODO"
      case EXTRACT => "TODO"
      case FLOAT_ABS =>
        callNode = createCall("<operator>.abs")
        handleSingleArgument(diffGraphBuilder, callNode, pcodeOp)
      case FLOAT_CEIL =>
        callNode = createCall("<operator>.ceil")
        handleSingleArgument(diffGraphBuilder, callNode, pcodeOp)
      case FLOAT_FLOAT2FLOAT =>
        callNode = createCall("<operator>.float2float")
        handleSingleArgument(diffGraphBuilder, callNode, pcodeOp)
      case FLOAT_FLOOR =>
        callNode = createCall("<operator>.floor")
        handleSingleArgument(diffGraphBuilder, callNode, pcodeOp)
      case FLOAT_INT2FLOAT =>
        callNode = createCall("<operator>.int2float")
        handleSingleArgument(diffGraphBuilder, callNode, pcodeOp)
      case FLOAT_LESS | INT_SLESS | INT_LESS =>
        callNode = createCall("<operator>.less")
        handleTwoArguments(diffGraphBuilder, nativeInstruction, callNode, pcodeOp, "<")
      case FLOAT_LESSEQUAL |INT_SLESSEQUAL | INT_LESSEQUAL  =>
        callNode = createCall("<operator>.lessThanEqual")
        handleTwoArguments(diffGraphBuilder, nativeInstruction, callNode, pcodeOp, "<=")
      case FLOAT_NAN =>
        callNode = createCall("<operator>.nan")
        handleSingleArgument(diffGraphBuilder, callNode, pcodeOp)
      case FLOAT_NOTEQUAL =>
        callNode = createCall("<operator>.notEqual")
        handleTwoArguments(diffGraphBuilder, nativeInstruction, callNode, pcodeOp, "!=")
      case FLOAT_ROUND => "TODO"
        callNode = createCall("<operator>.round")
        handleSingleArgument(diffGraphBuilder, callNode, pcodeOp)
      case FLOAT_SQRT => "TODO"
        callNode = createCall("<operator>.sqrt")
        handleSingleArgument(diffGraphBuilder, callNode, pcodeOp)
      case FLOAT_TRUNC => "TODO"
        callNode = createCall("<operator>.trunc")
        handleSingleArgument(diffGraphBuilder, callNode, pcodeOp)
      case INSERT => "TODO"
      case INT_2COMP => "TODO"
      case INT_ADD | FLOAT_ADD | PTRADD => 
        callNode = createCall("<operator>.addition")
        handleTwoArguments(diffGraphBuilder, nativeInstruction, callNode, pcodeOp, "+")
      case INT_AND => 
        callNode = createCall("<operator>.and")
        handleTwoArguments(diffGraphBuilder, nativeInstruction, callNode, pcodeOp, "&")
      case INT_CARRY => "TODO"
      case INT_DIV | FLOAT_DIV | INT_SDIV =>
        callNode = createCall("<operator>.division")
        handleTwoArguments(diffGraphBuilder, nativeInstruction, callNode, pcodeOp, "/")
      case FLOAT_EQUAL | INT_EQUAL | INT_NOTEQUAL  => nativeInstruction.toString
      case INT_LEFT => 
        callNode = createCall("<operator>.shiftleft")
        handleTwoArguments(diffGraphBuilder, nativeInstruction, callNode, pcodeOp, "<<")
      case INT_MULT | FLOAT_MULT =>
        callNode = createCall("<operator>.multiplication")
        handleTwoArguments(diffGraphBuilder, nativeInstruction, callNode, pcodeOp, "*")
      case FLOAT_NEG | INT_NEGATE => "TODO"
        callNode = createCall("<operator>.negate")
        handleSingleArgument(diffGraphBuilder, callNode, pcodeOp)
      case INT_OR =>
        callNode = createCall("<operator>.or")
        handleTwoArguments(diffGraphBuilder, nativeInstruction, callNode, pcodeOp, "|")
      case INT_REM | INT_SREM =>
        callNode = createCall("<operator>.modulo")
        handleTwoArguments(diffGraphBuilder, nativeInstruction, callNode, pcodeOp, "%")
      case INT_RIGHT | INT_SRIGHT =>
        callNode = createCall("<operator>.shiftRight")
        handleTwoArguments(diffGraphBuilder, nativeInstruction, callNode, pcodeOp, ">>")
      case INT_SBORROW => "TODO"
        //handleSingleArgument(diffGraphBuilder, nativeInstruction, callNode, pcodeOp, "%")
      case INT_SCARRY => "TODO"
      case INT_SEXT | INT_ZEXT=>
        callNode = createCall("<operator>.extend")
        handleSingleArgument(diffGraphBuilder, callNode, pcodeOp)
      case INT_SLESSEQUAL =>
        callNode = createCall("<operator>.lessEqual")
        handleTwoArguments(diffGraphBuilder, nativeInstruction, callNode, pcodeOp, "<=")
      case INT_SUB | FLOAT_SUB | PTRSUB =>
        callNode = createCall("<operator>.subtraction")
        handleTwoArguments(diffGraphBuilder, nativeInstruction, callNode, pcodeOp, "-")
      case INT_XOR =>
        callNode = createCall("<operator>.xor")
        handleTwoArguments(diffGraphBuilder, nativeInstruction, callNode, pcodeOp, "^^")
      case COPY | LOAD | STORE | SUBPIECE =>
        callNode = createCall("<operator>.assignment")
        //handleAssignment(diffGraphBuilder, nativeInstruction, callNode, pcodeOp, "*")
      case NEW =>
        callNode = createCall("<operator>.new")
        handleSingleArgument(diffGraphBuilder, callNode, pcodeOp)
      case RETURN => "RET" // TODO
      case UNIMPLEMENTED | SEGMENTOP | MULTIEQUAL | INDIRECT | PIECE | PCODE_MAX | POPCOUNT =>
        callNode = createCall("NOT HANDLED")
      case _ =>
        callNode = createCall("NOT HANDLED")
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

  def resolveVarNode(input: Varnode, index: Int): CfgNodeNew = {
    if (input.isRegister) {
      // we only care about the name
      createIdentifier(
        nativeInstruction.getProgram.getRegister(input).getName,
        nativeInstruction.getProgram.getRegister(input).getName,
        index + 1,
        Types.registerType(nativeInstruction.getProgram.getRegister(input).getName),
        input.getAddress.getOffsetAsBigInteger.intValue
      )
    } else if (input.isConstant || input.isAddress || input.isUnique) {
      createLiteral("0x" + input.getWordOffset.toHexString, index + 1, index + 1, Types.registerType(input.getWordOffset.toHexString), -1)
    } else {
      createLiteral("0x" + input.getWordOffset.toHexString, index + 1, index + 1, Types.registerType(input.getWordOffset.toHexString), -1)
    }
  }

  def connectCallToArgument(diffGraphBuilder: DiffGraphBuilder, call: CfgNodeNew, argument: CfgNodeNew): Unit = {
    diffGraphBuilder.addNode(argument)
    diffGraphBuilder.addEdge(call, argument, EdgeTypes.ARGUMENT)
    diffGraphBuilder.addEdge(call, argument, EdgeTypes.AST)
  }
}
