package io.joern.ghidra2cpg.passes.mips

import ghidra.app.decompiler.DecompInterface
import ghidra.program.model.address.GenericAddress
import ghidra.program.model.lang.Register
import ghidra.program.model.listing.{Function, Instruction, Program}
import ghidra.program.model.pcode.PcodeOp.{CALL, CALLIND}
import ghidra.program.model.scalar.Scalar
import ghidra.util.task.ConsoleTaskMonitor
import io.joern.ghidra2cpg.{Decompiler, Types}
import io.joern.ghidra2cpg.passes.FunctionPass
import io.joern.ghidra2cpg.processors.MipsProcessor
import io.joern.ghidra2cpg.utils.Nodes._
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

  def resolveCallArguments(instruction: Instruction, callNode: CfgNodeNew): Unit = {
    try {
      highFunction
        .getPcodeOps(instruction.getPcode.toList.last.getSeqnum.getTarget)
        .asScala
        .toList
        .head
        .getInputs
        // First input is the instruction
        // we have already handled it
        .drop(1)
        .zipWithIndex foreach {
        case (input, index) =>
          if (input.isRegister) {
            val name = input.getHigh.getName
            val node = createIdentifier(name,
                                        name,
                                        index,
                                        Types.registerType(name),
                                        instruction.getMinAddress.getOffsetAsBigInteger.intValue)
            addArgumentEdge(callNode, node)
          } else if (input.isConstant) {
            val node =
              createLiteral(input.getWordOffset.toHexString,
                            index,
                            index,
                            input.getWordOffset.toHexString,
                            instruction.getMinAddress.getOffsetAsBigInteger.intValue)
            addArgumentEdge(callNode, node)
          } else if (input.isUnique) {
            val value = address2Literal.getOrElse(input.getDef.getInputs.toList.head.getAddress.getOffset,
                                                  input.getDef.getInputs.toList.head.getAddress.getOffset.toString)
            val node = createLiteral(value,
                                     index,
                                     index,
                                     input.getWordOffset.toHexString,
                                     instruction.getMinAddress.getOffsetAsBigInteger.intValue)
            addArgumentEdge(callNode, node)
          } else {
            val node = createLiteral(input.toString(),
                                     index,
                                     index,
                                     input.toString(),
                                     instruction.getMinAddress.getOffsetAsBigInteger.intValue)
            addArgumentEdge(callNode, node)
          }
      }
    } catch {
      // jal 0x00000000
      case _: NoSuchElementException => logger.warn(s"Cannot resolve at ${instruction.getMinAddress}")
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
            addArgumentEdge(instructionNode, node)
          case scalar: Scalar =>
            val node = createLiteral(scalar.toString(16, false, false, "", ""),
                                     index + 1,
                                     index + 1,
                                     scalar.toString(16, false, false, "", ""),
                                     instruction.getMinAddress.getOffsetAsBigInteger.intValue)
            addArgumentEdge(instructionNode, node)
          case genericAddress: GenericAddress =>
            val node = createLiteral(genericAddress.toString,
                                     index + 1,
                                     index + 1,
                                     genericAddress.toString,
                                     instruction.getMinAddress.getOffsetAsBigInteger.intValue)
            addArgumentEdge(instructionNode, node)
          case _ =>
            logger.warn(
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

    instruction.getPcode.toList.last.getOpcode match {
      case CALL | CALLIND =>
        resolveCallArguments(instruction, callNode)
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
