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
  try {
    pcodeOps.foreach(getCallNode)
    //if (pcodeOps.lastOption.nonEmpty) {
    //  getCallNode(pcodeOps.last)
    //}
  } catch {
    case e: Exception => e.printStackTrace()
  }
  // needed by ghidra for decompiling reasons

  def getInstruction: Instruction = nativeInstruction

  def getPcodeOps: Array[PcodeOp] = pcodeOps

  def getOpcode: Int = pcodeOps.lastOption.get.getOpcode

  def getCallNode(pcodeOp: PcodeOp): CfgNodeNew = {
    val callCode = pcodeOp.getOpcode match {
      case BRANCH | BRANCHIND | CBRANCH => "<operator>.goto"
      case CALL | CALLOTHER | CALLIND => codeUnitFormat.getOperandRepresentationString(nativeInstruction, 0).split(">").last.replace("[", "").replace("]", "")
      case CAST => "TODO CAST"
      case COPY => "<operator>.assignment"
      case INT_ADD | FLOAT_ADD | PTRADD => "<operator>.addition"
      case INT_AND => "<operator>.and"
      case INT_DIV | FLOAT_DIV | INT_SDIV => "<operator>.division"
      case INT_EQUAL | INT_NOTEQUAL | INT_SLESS | INT_SLESSEQUAL | INT_LESS | INT_LESSEQUAL => nativeInstruction.toString
      case INT_MULT | FLOAT_MULT => "<operator>.multiplication"
      case INT_OR => "<operator>.or"
      case INT_SUB | FLOAT_SUB | PTRSUB => "<operator>.subtraction"
      case INT_XOR => "<operator>.xor"
      case LOAD => "<operator>.assignment"
      case MULTIEQUAL | INDIRECT | PIECE => "NOT HANDLED"
      case RETURN => "RET" // TODO
      case STORE => "<operator>.assignment"
      case SUBPIECE => "<operator>.assignment"
      case _ => "UNKNOWN"
      /* TODO:
          BOOL_AND BOOL_NEGATE BOOL_OR
          BOOL_XOR CPOOLREF EXTRACT
          FLOAT_ABS FLOAT_CEIL FLOAT_EQUAL
          FLOAT_FLOAT2FLOAT FLOAT_FLOOR FLOAT_INT2FLOAT
          FLOAT_LESS FLOAT_LESSEQUAL
          FLOAT_MULT FLOAT_NAN FLOAT_NEG
          FLOAT_NOTEQUAL FLOAT_ROUND
          FLOAT_SQRT FLOAT_TRUNC INSERT
          INT_2COMP INT_CARRY INT_EQUAL
          INT_LEFT INT_LESS INT_LESSEQUAL
          INT_MULT INT_NEGATE INT_NOTEQUAL
          INT_REM INT_RIGHT INT_SBORROW
          INT_SCARRY INT_SEXT INT_SLESS
          INT_SLESSEQUAL INT_SREM INT_SRIGHT
          INT_ZEXT MULTIEQUAL NEW PCODE_MAX
          POPCOUNT SEGMENTOP UNIMPLEMENTED
       */
    }
    val callNode = createCallNode(
      nativeInstruction.toString,
      callCode,
      nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue
    )
    if(pcodeOp.getOutput != null)
      resolvedPcodeInstructions += (pcodeOp.getOutput.toString -> callNode)
    //resolveArgument(pcodeOps.lastOption.get.getIn orNull)
    pcodeOp.getInputs.zipWithIndex.foreach { case (param, index) =>
      println("PARAM " + param)
      resolveArguments(diffGraphBuilder, param, callNode, index)
    }
    callNode
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
      val n = resolvedPcodeInstructions(input.toString)
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
