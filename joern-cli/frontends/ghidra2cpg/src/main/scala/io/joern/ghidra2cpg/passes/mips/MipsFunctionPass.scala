package io.joern.ghidra2cpg.passes.mips

import ghidra.app.decompiler.DecompInterface
import ghidra.program.model.address.GenericAddress
import ghidra.program.model.lang.Register
import ghidra.program.model.listing.{Function, Instruction, Program}
import ghidra.program.model.pcode.PcodeOp.{CALL, CALLIND}
import ghidra.program.model.scalar.Scalar
import ghidra.util.task.ConsoleTaskMonitor
import io.joern.ghidra2cpg.Types
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
                       decompInterface: DecompInterface)
    extends FunctionPass(new MipsProcessor, currentProgram, function, cpg, keyPool, decompInterface) {
  private val logger = LoggerFactory.getLogger(classOf[MipsFunctionPass])
  def resolveLiterals(instruction: Instruction, callNode: CfgNodeNew): Unit = {

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
          val name = currentProgram.getRegister(input.getAddress).getName
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
  }
  def addCallArguments(instruction: Instruction, callee: Function, callNode: CfgNodeNew): Unit = {
    // Array of tuples containing (checked parameter name, parameter index, parameter data type)
    var checkedParameters: Array[(String, Int, String)] = Array.empty
    if (callee.isThunk) {
      // thunk functions contain parameters already
      checkedParameters = callee.getParameters.map { parameter =>
        // checked parameter name, parameter index, parameter data type
        (Option(parameter.getName).getOrElse(parameter.getRegister.getName),
         parameter.getOrdinal + 1,
         parameter.getDataType.getName)
      }
    } else {
      // non thunk functions do not contain function parameters by default
      // need to decompile function to get parameter information
      // decompilation for a function is cached so subsequent calls to decompile should be free
      val parameters = decompInterface
        .decompileFunction(callee, 60, new ConsoleTaskMonitor())
        .getHighFunction
        .getLocalSymbolMap
        .getSymbols
        .asScala
        .toSeq
        .filter(_.isParameter)
        .toArray
      checkedParameters = parameters.map { parameter =>
        // checked parameter name, parameter index, parameter data type
        (Option(parameter.getName).getOrElse(parameter.getStorage.getRegister.getName),
         parameter.getCategoryIndex + 1,
         parameter.getDataType.getName)
      }
    }

    checkedParameters.foreach {
      case (checkedParameter, index, dataType) =>
        val node = createIdentifier(checkedParameter,
                                    checkedParameter,
                                    index,
                                    Types.registerType(dataType),
                                    instruction.getMinAddress.getOffsetAsBigInteger.intValue)
        addArgumentEdge(callNode, node)
    }

  }

  def addArguments(instruction: Instruction, instructionNode: CfgNodeNew): Unit = {
    for (index <- 0 until instruction.getNumOperands) {
      val opObjects = instruction.getOpObjects(index)
      if (opObjects.length > 1) {} else
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
    instruction.getPcode.toList.lastOption.getOrElse() match {
      case CALL | CALLIND =>
        // Call arguments are treated different
        val mipsPrefix = "^t9=>".r
        val calledFunction =
          mipsPrefix.replaceFirstIn(codeUnitFormat.getOperandRepresentationString(instruction, 0), "")
        functions.find(function => function.getName().equals(calledFunction)).foreach { callee =>
          // try to resolve literal arguments to call
          // eg. printf("foo")
          resolveLiterals(instruction, callNode)
          // resolve other arguments, like registers
          addCallArguments(instruction, callee, callNode)
        }
      case _ =>
        // regular instructions, eg. add/sub
        addArguments(instruction, callNode)
    }
  }
  override def runOnPart(part: String): Iterator[DiffGraph] = {
    methodNode = Some(
      createMethodNode(decompInterface, function, filename, checkIfExternal(currentProgram, function.getName)))
    diffGraph.addNode(methodNode.get)
    diffGraph.addNode(blockNode)
    diffGraph.addEdge(methodNode.get, blockNode, EdgeTypes.AST)
    val methodReturn = createReturnNode()
    diffGraph.addNode(methodReturn)
    diffGraph.addEdge(methodNode.get, methodReturn, EdgeTypes.AST)
    handleParameters()
    handleLocals()
    handleBody()
    Iterator(diffGraph.build())
  }
}
