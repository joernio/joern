package io.joern.ghidra2cpg.passes.mips

import ghidra.app.decompiler.DecompInterface
import ghidra.program.model.address.GenericAddress
import ghidra.program.model.lang.Register
import ghidra.program.model.listing.{Function, Instruction, Program}
import ghidra.program.model.scalar.Scalar
import ghidra.program.util.DefinedDataIterator
import ghidra.util.task.ConsoleTaskMonitor
import io.joern.ghidra2cpg.Types
import io.joern.ghidra2cpg.passes.FunctionPass
import io.joern.ghidra2cpg.processors.MipsProcessor
import io.joern.ghidra2cpg.utils.Nodes._
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.CfgNodeNew
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool}

import scala.jdk.CollectionConverters._
import scala.language.implicitConversions

class MipsFunctionPass(currentProgram: Program,
                       filename: String,
                       function: Function,
                       cpg: Cpg,
                       keyPool: IntervalKeyPool,
                       decompInterface: DecompInterface)
    extends FunctionPass(new MipsProcessor, currentProgram, filename, function, cpg, keyPool, decompInterface) {
  protected def findStringAtOffset(offset: Long) = {
    DefinedDataIterator
      .definedStrings(currentProgram)
      .iterator()
      .asScala
      .filter(x => x.getAddress().getOffset == offset)
      .toList
      .map(x => x.getValue.toString)
      .headOption
  }
  val mipsCallInstructions = List("jalr", "jal")
  // Iterating over operands and add edges to call
  override def handleArguments(
      instruction: Instruction,
      callNode: CfgNodeNew
  ): Unit = {
    val mnemonicString = instruction.getMnemonicString
    if (mipsCallInstructions.contains(mnemonicString)) {
      val mipsPrefix = "^t9=>".r
      val calledFunction =
        mipsPrefix.replaceFirstIn(codeUnitFormat.getOperandRepresentationString(instruction, 0), "")
      functions.find(function => function.getName().equals(calledFunction)).foreach { callee =>
        // Array of tuples containing (checked parameter name, parameter index, parameter data type)
        var checkedParameters: Array[(String, Int, String)] = Array.empty

        val instructionPcodes = highFunction
          .getPcodeOps(instruction.getPcode.toList.last.getSeqnum.getTarget)
          .asScala
          .toList
          .head
          .getInputs
          .drop(1)
        if (instructionPcodes.nonEmpty) {
          instructionPcodes.zipWithIndex foreach {
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
                val node = nodes
                  .NewLiteral()
                  .code(input.getWordOffset.toHexString)
                  .order(index)
                  .argumentIndex(index)
                  .typeFullName(input.getWordOffset.toHexString)
                addArgumentEdge(callNode, node)
              } else if (input.isUnique) {
                val value = findStringAtOffset(input.getDef.getInputs.toList.head.getAddress.getOffset + 8)
                  .getOrElse(findStringAtOffset(input.getDef.getInputs.toList.head.getAddress.getOffset)
                    .getOrElse(input.getDef.getInputs.toList.head.getAddress.getOffset))
                val node = nodes
                  .NewLiteral()
                  .code(value.toString)
                  .order(index)
                  .argumentIndex(index)
                  .typeFullName(input.getWordOffset.toHexString)
                addArgumentEdge(callNode, node)
              } else {
                val node = nodes
                  .NewLiteral()
                  .code(input.toString)
                  .order(index)
                  .argumentIndex(index)
                  .typeFullName(input.toString)
                addArgumentEdge(callNode, node)
              }
          }
        }
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
    } else {
      for (index <- 0 until instruction.getNumOperands) {
        val opObjects = instruction.getOpObjects(index)
        if (opObjects.length > 1) {
          val argument = String.valueOf(
            instruction.getDefaultOperandRepresentation(index)
          )
          val node = createIdentifier(argument,
                                      argument,
                                      index + 1,
                                      Types.registerType(argument),
                                      instruction.getMinAddress.getOffsetAsBigInteger.intValue)
          addArgumentEdge(callNode, node)
        } else
          for (opObject <- opObjects) { //
            val className = opObject.getClass.getSimpleName
            opObject.getClass.getSimpleName match {
              case "Register" =>
                val register = opObject.asInstanceOf[Register]
                val node = createIdentifier(register.getName,
                                            register.getName,
                                            index + 1,
                                            Types.registerType(register.getName),
                                            instruction.getMinAddress.getOffsetAsBigInteger.intValue)
                addArgumentEdge(callNode, node)
              case "Scalar" =>
                val scalar =
                  opObject.asInstanceOf[Scalar].toString(16, false, false, "", "")
                val node = nodes
                  .NewLiteral()
                  .code(scalar)
                  .order(index + 1)
                  .argumentIndex(index + 1)
                  .typeFullName(scalar)
                  .lineNumber(Some(instruction.getMinAddress.getOffsetAsBigInteger.intValue))
                addArgumentEdge(callNode, node)
              case "GenericAddress" =>
                // TODO: try to resolve the address
                val genericAddress =
                  opObject.asInstanceOf[GenericAddress].toString()
                val node = nodes
                  .NewLiteral()
                  .code(genericAddress)
                  .order(index + 1)
                  .argumentIndex(index + 1)
                  .typeFullName(genericAddress)
                  .lineNumber(Some(instruction.getMinAddress.getOffsetAsBigInteger.intValue))
                addArgumentEdge(callNode, node)
              case _ =>
                println(
                  s"""Unsupported argument: $opObject $className"""
                )
            }
          }
      }
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
