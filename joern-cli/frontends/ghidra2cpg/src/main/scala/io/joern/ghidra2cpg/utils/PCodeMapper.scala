package io.joern.ghidra2cpg.utils

import ghidra.program.model.listing.{CodeUnitFormat, CodeUnitFormatOptions, Instruction}
import ghidra.program.model.pcode.PcodeOp._
import ghidra.program.model.pcode.{PcodeOp, Varnode}
import io.joern.ghidra2cpg.utils.Utils.createCallNode
import io.shiftleft.codepropertygraph.generated.nodes.CfgNodeNew

import scala.collection.mutable
import scala.collection.mutable.HashMap
/*
  TODO: resolve the unique arguments of the pcodeOps
 */
class PCodeMapper(nativeInstruction: Instruction) {
  var resolvedPcodeInstructions: HashMap[String, String] = new mutable.HashMap[String, String]()
  private val pcodeOps: Array[PcodeOp]                   = nativeInstruction.getPcode()
  try {
    if (pcodeOps.lastOption.nonEmpty) {
      getCallNode(pcodeOps.last)
    }
  } catch {
    case e: Exception => e.printStackTrace()
  }
  // needed by ghidra for decompiling reasons
  protected val codeUnitFormat = new CodeUnitFormat(
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
  resolveArgument(pcodeOps.lastOption.orNull)

  def getInstruction: Instruction = nativeInstruction
  def getPcodeOps: Array[PcodeOp] = pcodeOps
  def getOpcode: Int              = pcodeOps.lastOption.get.getOpcode

  def getCallNode(pcodeOp: PcodeOp): CfgNodeNew = {
    pcodeOp.getOpcode match {
      case INT_EQUAL | INT_NOTEQUAL | INT_SLESS | INT_SLESSEQUAL | INT_LESS | INT_LESSEQUAL =>
        createCallNode(
          nativeInstruction.toString,
          nativeInstruction.toString,
          nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue
        )
      case COPY =>
        createCallNode(
          nativeInstruction.toString,
          "<operator>.assignment",
          nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue
        )
      case LOAD =>
        createCallNode(
          nativeInstruction.toString,
          "<operator>.assignment",
          nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue
        )
      // handleAssignment(diffGraphBuilder, nativeInstruction, callNode, pcodeOp.get)
      case SUBPIECE =>
        createCallNode(
          nativeInstruction.toString,
          "<operator>.assignment",
          nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue
        )
      case STORE =>
        createCallNode(
          nativeInstruction.toString,
          "<operator>.assignment",
          nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue
        )
      case CALL =>
        var calledFunction = "UNKNOWN"
        try {
          calledFunction = codeUnitFormat.getOperandRepresentationString(nativeInstruction, 1)
        } catch {
          case e: Exception => // e.printStackTrace()
        }
        createCallNode(calledFunction, calledFunction, nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue)
      case CALLOTHER =>
        var calledFunction = "UNKNOWN"
        try {
          calledFunction = codeUnitFormat.getOperandRepresentationString(nativeInstruction, 1)
        } catch {
          case e: Exception => // e.printStackTrace()
        }
        createCallNode(calledFunction, calledFunction, nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue)
      case CALLIND =>
        var calledFunction = "UNKNOWN"
        try {

          calledFunction = codeUnitFormat
            .getOperandRepresentationString(nativeInstruction, 1)
            .split(">")
            .last
            .replace("[", "")
            .replace("]", "")
        } catch {
          case e: Exception => // e.printStackTrace()
        }
        createCallNode(calledFunction, calledFunction, nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue)
      case INT_ADD | FLOAT_ADD | PTRADD =>
        createCallNode(
          nativeInstruction.toString,
          "<operator>.addition",
          nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue
        )
      case RETURN =>
        createCallNode(
          nativeInstruction.toString,
          "RET",
          nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue
        )
      case INT_AND =>
        createCallNode(
          nativeInstruction.toString,
          "<operator>.and",
          nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue
        )
      case INT_OR =>
        createCallNode(
          nativeInstruction.toString,
          "<operator>.or",
          nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue
        )
      case INT_DIV | FLOAT_DIV | INT_SDIV =>
        createCallNode(
          nativeInstruction.toString,
          "<operator>.division",
          nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue
        )
      case INT_SUB | FLOAT_SUB | PTRSUB =>
        createCallNode(
          nativeInstruction.toString,
          "<operator>.subtraction",
          nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue
        )

      case INT_MULT | FLOAT_MULT =>
        createCallNode(
          nativeInstruction.toString,
          "<operator>.multiplication",
          nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue
        )

      case MULTIEQUAL | INDIRECT | PIECE => // not handled
        Utils.createCallNode("", "", -1)
      case INT_XOR =>
        createCallNode(
          nativeInstruction.toString,
          "<operator>.xor",
          nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue
        )

      case CAST =>
        // println("TODO: CAST")
        createCallNode("UNKNOWN", "UNKNOWN", nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue)
      // we need to "unpack" the def of the first input of the cast
      // eg. "(param_1 + 5)" in "(void *)(param_1 + 5)"
      // if (pcodeOp.getInput(0).getDef != null)
      //  resolveArgument(diffGraphBuilder, nativeInstruction, opNode, pcodeOps.last.getInput(0).getDef, index)
      case BRANCH | BRANCHIND | CBRANCH =>
        createCallNode(
          nativeInstruction.toString,
          "<operator>.goto",
          nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue
        )
      case _ => createCallNode("UNKNOWN", "UNKNOWN", nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue)
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
  }

  private def resolveArgument(pcodeOp: PcodeOp): String = {
    // sometimes there no pcodes
    if (pcodeOp == null) return "no"
    // Iterating over the parameter of the last nativeInstruction
    pcodeOp.getInputs.foreach { input =>
      if (input.isRegister) {
        // we only care about the name
        return nativeInstruction.getProgram.getRegister(input).getName
      } else if (input.isUnique) {
        // TODO: Check the list of pcodes if the unique is present
        println(pcodeOp.getInputs.map(_.getDef).mkString("   ;;   "))
        "Todo resolve unique"
      } else if (input.isConstant) {
        "0x" + input.getWordOffset.toHexString
      } else if (input.isAddress) {
        "0x" + input.getWordOffset.toHexString
      } else {
        println(" UNKNOWN " + input.toString())
        "UNKNOWN"
      }
    }
    ""
  }
  // }
}

//private def resolveVarNode(varNode: Varnode): String = {
//  if (varNode.isRegister)
//    return nativeInstruction.getProgram.getRegister(varNode).getName
//  if (varNode.isConstant)
//    return "0x" + varNode.getWordOffset.toHexString
//  if (varNode.isAddress) {
//    // TODO: resolve the address?
//    return varNode.getAddress.toString
//  }
//  if (varNode.isUnique)
//    return "0x" + varNode.getWordOffset.toHexString
//  "TODO: " + varNode.toString
//}
