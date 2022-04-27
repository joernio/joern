package io.joern.ghidra2cpg.utils

import ghidra.program.model.listing.Instruction
import ghidra.program.model.pcode.{PcodeOp, Varnode}

class PCodeMapper(nativeInstruction: Instruction) {

  private val pcodeOps: Array[PcodeOp] = nativeInstruction.getPcode()
  pcodeOps.dropRight(1).foreach(resolveArgument)
  def getInstruction: Instruction      = nativeInstruction
  def getPcodeOps: Array[PcodeOp]      = pcodeOps
  def getOpcode: Int                   = pcodeOps.lastOption.get.getOpcode

  private def resolveVarNode(varNode: Varnode): String = {
    if(varNode.isRegister)
      return nativeInstruction.getProgram.getRegister(varNode).getName
    if(varNode.isConstant)
      return "0x"+varNode.getWordOffset.toHexString
    if(varNode.isAddress) {
      // TODO: resolve the address?
      return varNode.getAddress.toString
    }
    if(varNode.isUnique)
      return "0x"+varNode.getWordOffset.toHexString
    "TODO: "+varNode.toString
  }

  def getResolvedPcodeInstruction(): Unit = {

  }

  private def resolveArgument(pcodeOp: PcodeOp): Unit            = {
      if(pcodeOp.getOpcode == PcodeOp.STORE) {
        println("NULL " + pcodeOp)
      } else if(pcodeOp.getOutput.isRegister){
        //
        val ret = nativeInstruction.getProgram.getRegister(pcodeOp.getOutput)
        //println(pcodeOp.getMnemonic)
        if(pcodeOp.getOpcode == PcodeOp.COPY) {
          val d = ret + "=" +  pcodeOp.getInputs.map(x=>resolveVarNode(x)).mkString(" ")
          println("FFFFF " + d)
        }
        pcodeOp.getInputs.map(resolveVarNode)//.mkString(" ; "))
      }
      else if(pcodeOp.getOutput.isUnique){
        println("ISUNIQUE " + pcodeOp)
      } else {
        println("ELSE " + pcodeOp)
      }
  }
}
