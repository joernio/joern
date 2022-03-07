package io.joern.ghidra2cpg.passes.x86

import ghidra.program.model.address.GenericAddress
import ghidra.program.model.lang.Register
import ghidra.program.model.listing.{CodeUnitFormat, CodeUnitFormatOptions, Function, FunctionIterator, Instruction, Listing, Program}
import ghidra.program.model.pcode.HighFunction
import ghidra.program.model.scalar.Scalar
import io.joern.ghidra2cpg.processors.X86Processor
import io.joern.ghidra2cpg.utils.Nodes._
import io.joern.ghidra2cpg.{Decompiler, Types}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNodeNew, NewBlock, NewMethod}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}
import io.shiftleft.passes.ConcurrentWriterCpgPass

import scala.collection.JavaConverters.asScalaIteratorConverter
import scala.language.implicitConversions

class X86FunctionPass(currentProgram: Program, filename: String, function: Function, cpg: Cpg, decompiler: Decompiler)
  extends ConcurrentWriterCpgPass[Function](cpg) {
  val processor = new X86Processor()
  val listing: Listing = currentProgram.getListing
  val functionIterator: FunctionIterator = listing.getFunctions(true)
  val functions: List[Function] = functionIterator.iterator.asScala.toList
  val highFunction: HighFunction = decompiler.toHighFunction(function).orNull
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
  protected var methodNode: Option[NewMethod] = None

  // override def partIterator: Iterator[Method] = cpg.method.l.iterator

  implicit def intToIntegerOption(intOption: Option[Int]): Option[Integer] = {
    intOption.map(intValue => {
      val integerValue: Integer = intValue
      integerValue
    })
  }

  override def runOnPart(diffGraphBuilder: DiffGraphBuilder, function: Function): Unit = {
    methodNode = Some(
      createMethodNode(decompiler, function, filename, checkIfExternal(currentProgram, function.getName))
    )
    diffGraphBuilder.addNode(methodNode.get)
    diffGraphBuilder.addNode(blockNode)
    diffGraphBuilder.addEdge(methodNode.get, blockNode, EdgeTypes.AST)
    val methodReturn = createReturnNode()
    diffGraphBuilder.addNode(methodReturn)
    diffGraphBuilder.addEdge(methodNode.get, methodReturn, EdgeTypes.AST)
    handleParameters(diffGraphBuilder)
    handleLocals(diffGraphBuilder)
    handleBody(diffGraphBuilder)
  }

  def handleParameters(diffGraphBuilder: DiffGraphBuilder): Unit = {
    if (function.isThunk) {
      function
        .getThunkedFunction(true)
        .getParameters
        .zipWithIndex
        .foreach { case (parameter, index) =>
          val node = createParameterNode(
            parameter.getName,
            parameter.getName,
            index + 1,
            parameter.getDataType.getName,
            function.getEntryPoint.getOffsetAsBigInteger.intValue()
          )
          diffGraphBuilder.addNode(node)
          diffGraphBuilder.addEdge(methodNode.get, node, EdgeTypes.AST)
        }
    } else {
      highFunction.getLocalSymbolMap.getSymbols.asScala.toSeq
        .filter(_.isParameter)
        .foreach { parameter =>
          val checkedParameter = Option(parameter.getStorage)
            .flatMap(x => Option(x.getRegister))
            .flatMap(x => Option(x.getName))
            .getOrElse(parameter.getName)
          val node =
            createParameterNode(
              checkedParameter,
              checkedParameter,
              parameter.getCategoryIndex + 1,
              parameter.getDataType.getName,
              function.getEntryPoint.getOffsetAsBigInteger.intValue()
            )
          diffGraphBuilder.addNode(node)
          diffGraphBuilder.addEdge(methodNode.get, node, EdgeTypes.AST)
        }
    }
  }

  def handleLocals(diffGraphBuilder: DiffGraphBuilder): Unit = {
    function.getLocalVariables.foreach { local =>
      val localNode = nodes
        .NewLocal()
        .name(local.getName)
        .code(local.toString)
        .typeFullName(Types.registerType(local.getDataType.toString))
      val identifier =
        createIdentifier(local.getName, local.getSymbol.getName, -1, local.getDataType.toString, -1)

      diffGraphBuilder.addNode(localNode)
      diffGraphBuilder.addNode(identifier)
      diffGraphBuilder.addEdge(blockNode, localNode, EdgeTypes.AST)
      diffGraphBuilder.addEdge(blockNode, identifier, EdgeTypes.AST)
      diffGraphBuilder.addEdge(identifier, localNode, EdgeTypes.REF)
    }
  }

  def handleBody(diffGraphBuilder: DiffGraphBuilder): Unit = {
    if (instructions.nonEmpty) {
      var prevInstructionNode = addCallOrReturnNode(instructions.head)
      handleArguments(diffGraphBuilder, instructions.head, prevInstructionNode)
      diffGraphBuilder.addEdge(blockNode, prevInstructionNode, EdgeTypes.AST)
      diffGraphBuilder.addEdge(methodNode.get, prevInstructionNode, EdgeTypes.CFG)
      instructions.drop(1).foreach { instruction =>
        val instructionNode = addCallOrReturnNode(instruction)
        diffGraphBuilder.addNode(instructionNode)
        handleArguments(diffGraphBuilder, instruction, instructionNode)
        diffGraphBuilder.addEdge(blockNode, instructionNode, EdgeTypes.AST)
        // Not connecting the previous instruction,
        // if it is an unconditional jump
        // JMP is x86 specific
        if (!prevInstructionNode.code.startsWith("JMP")) {
          diffGraphBuilder.addEdge(prevInstructionNode, instructionNode, EdgeTypes.CFG)
        }
        prevInstructionNode = instructionNode
      }
    }
  }

  // Iterating over operands and add edges to call
  def handleArguments(diffGraphBuilder: DiffGraphBuilder, instruction: Instruction, callNode: CfgNodeNew): Unit = {
    val mnemonicString = processor.getInstructions(instruction.getMnemonicString)
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
            instruction.getMinAddress.getOffsetAsBigInteger.intValue
          )
          connectCallToArgument(diffGraphBuilder, callNode, node)
        }
      }
    } else {
      for (index <- 0 until instruction.getNumOperands) {
        val opObjects = instruction.getOpObjects(index)
        if (opObjects.length > 1) {
          val argument = String.valueOf(instruction.getDefaultOperandRepresentation(index))
          val node = createIdentifier(
            argument,
            argument,
            index + 1,
            Types.registerType(argument),
            instruction.getMinAddress.getOffsetAsBigInteger.intValue
          )
          connectCallToArgument(diffGraphBuilder, callNode, node)
        } else
          for (opObject <- opObjects) { //
            val className = opObject.getClass.getSimpleName
            opObject.getClass.getSimpleName match {
              case "Register" =>
                val register = opObject.asInstanceOf[Register]
                val node = createIdentifier(
                  register.getName,
                  register.getName,
                  index + 1,
                  Types.registerType(register.getName),
                  instruction.getMinAddress.getOffsetAsBigInteger.intValue
                )
                connectCallToArgument(diffGraphBuilder, callNode, node)
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
                connectCallToArgument(diffGraphBuilder, callNode, node)
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
                connectCallToArgument(diffGraphBuilder, callNode, node)
              case _ =>
                println(s"""Unsupported argument: $opObject $className""")
            }
          }
      }
    }
  }

  def connectCallToArgument(diffGraphBuilder: DiffGraphBuilder, call: CfgNodeNew, argument: CfgNodeNew): Unit = {
    diffGraphBuilder.addNode(argument)
    diffGraphBuilder.addEdge(call, argument, EdgeTypes.ARGUMENT)
    diffGraphBuilder.addEdge(call, argument, EdgeTypes.AST)
  }

  def addCallOrReturnNode(instruction: Instruction): CfgNodeNew =
    processor.getInstructions
      .getOrElse(instruction.getMnemonicString, "UNKNOWN") match {
      case "RET" =>
        createReturnNode(instruction.toString, instruction.getMinAddress.getOffsetAsBigInteger.intValue())
      case "CALL" | "LEAVE" =>
        val code = sanitizeMethodName(codeUnitFormat.getOperandRepresentationString(instruction, 0))
        createCallNode(code, code, instruction.getMinAddress.getOffsetAsBigInteger.intValue())
      case "UNKNOWN" =>
        createCallNode(instruction.toString, "UNKNOWN", instruction.getMinAddress.getOffsetAsBigInteger.intValue())
      case operator =>
        createCallNode(instruction.toString, operator, instruction.getMinAddress.getOffsetAsBigInteger.intValue())
    }

  def sanitizeMethodName(methodName: String): String = {
    methodName.split(">").lastOption.getOrElse(methodName).replace("[", "").replace("]", "")
  }

}
