package io.joern.ghidra2cpg.passes.mips

import ghidra.app.decompiler.DecompInterface
import ghidra.program.model.address.GenericAddress
import ghidra.program.model.lang.Register
import ghidra.program.model.listing.{Function, Instruction, Program}
import ghidra.program.model.scalar.Scalar
import ghidra.util.task.ConsoleTaskMonitor
import io.joern.ghidra2cpg.Types
import io.joern.ghidra2cpg.passes.FunctionPass
import io.joern.ghidra2cpg.processors.MipsProcessor
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.codepropertygraph.generated.nodes.NewCall
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

  val mipsCallInstructions = List("jalr", "jal")
  // Iterating over operands and add edges to call
  override def handleArguments(
      instruction: Instruction,
      callNode: NewCall
  ): Unit = {
    val mnemonicString = instruction.getMnemonicString
    if (mipsCallInstructions.contains(mnemonicString)) {
      val mipsPrefix = "^t9=>".r
      val calledFunction =
        mipsPrefix.replaceFirstIn(codeUnitFormat.getOperandRepresentationString(instruction, 0), "")
      functions.find(function => function.getName().equals(calledFunction)).foreach { callee =>
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
                                        Some(instruction.getMinAddress.getOffsetAsBigInteger.intValue))
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
                                      Some(instruction.getMinAddress.getOffsetAsBigInteger.intValue))
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
                                            Some(instruction.getMinAddress.getOffsetAsBigInteger.intValue))
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
    createMethodNode()
    handleParameters()
    handleLocals()
    handleBody()
    Iterator(diffGraph.build())
  }
}
