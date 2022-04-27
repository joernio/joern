package io.joern.ghidra2cpg.utils

import ghidra.program.model.listing.Instruction
import ghidra.program.model.pcode.PcodeOp

class PCodeMapper(nativeInstruction: Instruction) {
  private val pcodeOps: Array[PcodeOp] = nativeInstruction.getPcode()
  def getInstruction: Instruction      = nativeInstruction
  def getPcodeOps: Array[PcodeOp]      = pcodeOps
  def getOpcode: Int                   = pcodeOps.lastOption.get.getOpcode

  def getResolvedPcodeInstruction(): Unit = {}
  def resolveArguments(): Unit            = {}
}
