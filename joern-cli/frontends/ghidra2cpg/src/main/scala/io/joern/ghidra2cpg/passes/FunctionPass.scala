package io.joern.ghidra2cpg.passes

import ghidra.app.decompiler.DecompInterface
import ghidra.program.model.address.GenericAddress
import ghidra.program.model.lang.Register
import ghidra.program.model.listing.{
  CodeUnitFormat,
  CodeUnitFormatOptions,
  Function,
  FunctionIterator,
  Instruction,
  Listing,
  Program
}
import ghidra.program.model.scalar.Scalar
import ghidra.util.task.ConsoleTaskMonitor
import io.joern.ghidra2cpg._
import io.joern.ghidra2cpg.processors._
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{NewBlock, NewCall, NewMethod, NewNode}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes, nodes}
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}
import io.shiftleft.proto.cpg.Cpg.DispatchTypes

import scala.jdk.CollectionConverters._
import scala.language.implicitConversions

abstract class FunctionPass(
    processor: Processor,
    currentProgram: Program,
    filename: String,
    function: Function,
    cpg: Cpg,
    keyPool: IntervalKeyPool,
    decompInterface: DecompInterface
) extends ParallelCpgPass[String](
      cpg,
      keyPools = Some(keyPool.split(1))
    ) {
  implicit val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder

  val listing: Listing = currentProgram.getListing
  val functionIterator: FunctionIterator = listing.getFunctions(true)
  val functions: List[Function] = functionIterator.iterator.asScala.toList

  protected var methodNode: Option[NewMethod] = None
  // we need it just once with default settings
  protected val blockNode: NewBlock = nodes.NewBlock().code("").order(0)

  protected val instructions: Seq[Instruction] =
    currentProgram.getListing.getInstructions(function.getBody, true).iterator().asScala.toList
  // needed by ghidra for decompiling reasons
  protected val codeUnitFormat: CodeUnitFormat = new CodeUnitFormat(
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

  override def partIterator: Iterator[String] = List("").iterator

  implicit def intToIntegerOption(intOption: Option[Int]): Option[Integer] = {
    intOption.map(intValue => {
      val integerValue: Integer = intValue
      integerValue
    })
  }

  def handleParameters(): Unit = {
    if (function.isThunk) {
      function
        .getThunkedFunction(true)
        .getParameters
        .zipWithIndex
        .foreach {
          case (parameter, index) =>
            val node = nodes
              .NewMethodParameterIn()
              .code(parameter.getName)
              .name(parameter.getName)
              .order(index + 1)
              .typeFullName(Types.registerType(parameter.getDataType.getName))
              .lineNumber(Some(function.getEntryPoint.getOffsetAsBigInteger.intValue()))
            diffGraph.addNode(node)
            diffGraph.addEdge(methodNode.get, node, EdgeTypes.AST)
        }
    } else {
      decompInterface
        .decompileFunction(function, 60, new ConsoleTaskMonitor())
        .getHighFunction
        .getLocalSymbolMap
        .getSymbols
        .asScala
        .toSeq
        .filter(_.isParameter)
        .foreach { parameter =>
          var checkedParameter = ""
          if (parameter.getStorage.getRegister == null) {
            checkedParameter = parameter.getName
          } else {
            checkedParameter = parameter.getStorage.getRegister.getName
          }
          val node = nodes
            .NewMethodParameterIn()
            .code(checkedParameter)
            .name(checkedParameter)
            .order(parameter.getCategoryIndex + 1)
            .typeFullName(Types.registerType(parameter.getDataType.getName))
            .lineNumber(Some(function.getEntryPoint.getOffsetAsBigInteger.intValue()))
          diffGraph.addNode(node)
          diffGraph.addEdge(methodNode.get, node, EdgeTypes.AST)
        }
    }
  }

  def handleLocals(): Unit = {
    function.getLocalVariables.foreach { local =>
      val localNode = nodes
        .NewLocal()
        .name(local.getName)
        .code(local.toString)
        .typeFullName(Types.registerType(local.getDataType.toString))
      val identifier = nodes
        .NewIdentifier()
        .code(local.getName)
        .name(local.getSymbol.getName)
        .typeFullName(local.getDataType.toString)

      diffGraph.addNode(localNode)
      diffGraph.addNode(identifier)
      diffGraph.addEdge(blockNode, localNode, EdgeTypes.AST)
      diffGraph.addEdge(blockNode, identifier, EdgeTypes.AST)
      diffGraph.addEdge(identifier, localNode, EdgeTypes.REF)
    }
  }

  def handleBody(): Unit = {
    if (instructions.nonEmpty) {
      var prevInstructionNode = addCallNode(instructions.head)
      handleArguments(instructions.head, prevInstructionNode)
      diffGraph.addEdge(blockNode, prevInstructionNode, EdgeTypes.AST)
      diffGraph.addEdge(methodNode.get, prevInstructionNode, EdgeTypes.CFG)
      instructions.drop(1).foreach { instruction =>
        val instructionNode = addCallNode(instruction)
        diffGraph.addNode(instructionNode)
        handleArguments(instruction, instructionNode)
        diffGraph.addEdge(blockNode, instructionNode, EdgeTypes.AST)
        diffGraph.addEdge(prevInstructionNode, instructionNode, EdgeTypes.CFG)
        prevInstructionNode = instructionNode
      }
    }
  }

  // Iterating over operands and add edges to call
  def handleArguments(
      instruction: Instruction,
      callNode: NewCall
  ): Unit = {
    val mnemonicString = instruction.getMnemonicString
    if (mnemonicString.equals("CALL")) {
      val calledFunction =
        codeUnitFormat.getOperandRepresentationString(instruction, 0)
      val callee = functions.find(function => function.getName().equals(calledFunction))
      if (callee.nonEmpty) {
        // Array of tuples containing (checked parameter name, parameter index, parameter data type)
        var checkedParameters: Array[(String, Int, String)] = Array.empty

        if (callee.head.isThunk) {
          // thunk functions contain parameters already
          val parameters = callee.head.getParameters

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
          val parameters = decompInterface
            .decompileFunction(callee.head, 60, new ConsoleTaskMonitor())
            .getHighFunction
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

        checkedParameters.foreach {
          case (checkedParameter, index, dataType) =>
            val node = nodes
              .NewIdentifier()
              .code(checkedParameter)
              .name(checkedParameter) //parameter.getName)
              .order(index)
              .argumentIndex(index)
              .typeFullName(Types.registerType(dataType))
              .lineNumber(Some(instruction.getMinAddress.getOffsetAsBigInteger.intValue))
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
          val node = nodes
            .NewIdentifier()
            .code(argument)
            .name(argument)
            .order(index + 1)
            .argumentIndex(index + 1)
            .typeFullName(Types.registerType(argument))
            .lineNumber(Some(instruction.getMinAddress.getOffsetAsBigInteger.intValue))
          addArgumentEdge(callNode, node)
        } else
          for (opObject <- opObjects) { //
            val className = opObject.getClass.getSimpleName
            opObject.getClass.getSimpleName match {
              case "Register" =>
                val register = opObject.asInstanceOf[Register]
                val node = nodes
                  .NewIdentifier()
                  .code(register.getName)
                  .name(register.getName)
                  .order(index + 1)
                  .argumentIndex(index + 1)
                  .typeFullName(Types.registerType(register.getName))
                  .lineNumber(Some(instruction.getMinAddress.getOffsetAsBigInteger.intValue))
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

  def addArgumentEdge(fromNode: NewNode, toNode: NewNode): Unit = {
    diffGraph.addNode(toNode)
    diffGraph.addEdge(fromNode, toNode, EdgeTypes.ARGUMENT)
    diffGraph.addEdge(fromNode, toNode, EdgeTypes.AST)
  }

  def createCallNode(code: String, mnemonic: String, lineNumber: Integer): NewCall = {
    nodes
      .NewCall()
      .name(mnemonic)
      .code(code)
      .order(0)
      .methodFullName(mnemonic)
      .dispatchType(DispatchTypes.STATIC_DISPATCH.name())
      .lineNumber(lineNumber)
  }

  def addCallNode(instruction: Instruction): NewCall = {
    processor.getInstructions
      .getOrElse(instruction.getMnemonicString, "UNKNOWN") match {
      case "LEAVE" | "RET" =>
        createCallNode("RET", "RET", instruction.getMinAddress.getOffsetAsBigInteger.intValue())
      case "CALL" =>
        val code = sanitizeMethodName(
          codeUnitFormat.getOperandRepresentationString(instruction, 0)
        )
        createCallNode(code, code, instruction.getMinAddress.getOffsetAsBigInteger.intValue())
      case "UNKNOWN" =>
        createCallNode(instruction.toString, "UNKNOWN", instruction.getMinAddress.getOffsetAsBigInteger.intValue())
      case operator =>
        createCallNode(instruction.toString, operator, instruction.getMinAddress.getOffsetAsBigInteger.intValue())
    }
  }

  def sanitizeMethodName(methodName: String): String = {
    methodName.split(">").lastOption.getOrElse(methodName).replace("[", "").replace("]", "")
  }

  def createMethodNode(): Unit = {
    methodNode = Some(
      nodes
        .NewMethod()
        .code(function.getName)
        .name(function.getName)
        .fullName(function.getSignature(true).toString)
        .isExternal(checkIfExternal(function.getName))
        .signature(function.getSignature(true).toString)
        .lineNumber(Some(function.getEntryPoint.getOffsetAsBigInteger.intValue()))
        .columnNumber(Some(-1))
        .lineNumberEnd(Some(function.getReturn.getMinAddress.getOffsetAsBigInteger.intValue()))
        .order(0)
        .filename(filename)
        .astParentType(NodeTypes.NAMESPACE_BLOCK)
        .astParentFullName(s"$filename:<global>")
    )

    diffGraph.addNode(methodNode.get)
    diffGraph.addNode(blockNode)
    diffGraph.addEdge(methodNode.get, blockNode, EdgeTypes.AST)

    // We need at least one of "NewMethodReturn"
    val methodReturn = nodes.NewMethodReturn().order(1)
    diffGraph.addNode(methodReturn)
    diffGraph.addEdge(methodNode.get, methodReturn, EdgeTypes.AST)
  }

  def checkIfExternal(functionName: String): Boolean = {
    currentProgram.getFunctionManager.getExternalFunctions
      .iterator()
      .asScala
      .map(_.getName)
      .contains(functionName)
  }
}
