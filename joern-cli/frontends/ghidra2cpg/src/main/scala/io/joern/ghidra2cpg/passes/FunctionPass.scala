package io.joern.ghidra2cpg.passes

import ghidra.app.util.template.TemplateSimplifier
import ghidra.program.model.address.GenericAddress
import ghidra.program.model.lang.Register
import ghidra.program.model.listing.{CodeUnitFormat, CodeUnitFormatOptions, Function, Instruction, Program}
import ghidra.program.model.pcode.{HighFunction, HighSymbol}
import ghidra.program.model.scalar.Scalar
import io.joern.ghidra2cpg._
import io.joern.ghidra2cpg.processors._
import io.joern.ghidra2cpg.utils.Decompiler
import io.joern.ghidra2cpg.utils.Utils._
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNodeNew, NewBlock, NewMethod}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}
import io.shiftleft.passes.ForkJoinParallelCpgPass

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.language.implicitConversions

abstract class FunctionPass(
  processor: Processor,
  currentProgram: Program,
  functions: List[Function],
  cpg: Cpg,
  decompiler: Decompiler
) extends ForkJoinParallelCpgPass[Function](cpg) {

  protected val functionByName: mutable.Map[String, Function] = mutable.HashMap[String, Function]()
  for (fn <- functions) {
    val other = functionByName.getOrElseUpdate(fn.getName, fn)
    if (!(other eq fn)) {
      baseLogger.warn(s"Multiple functions with same name ${fn.getName}, can't disambiguate: $fn, $other")
    }
  }

  def getHighFunction(function: Function): Option[HighFunction] =
    decompiler.toHighFunction(function)

  protected def getInstructions(function: Function): Seq[Instruction] =
    currentProgram.getListing.getInstructions(function.getBody, true).iterator().asScala.toList

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
      true,
      new TemplateSimplifier()
    )
  )

  override def generateParts(): Array[Function] = functions.toArray

  implicit def intToIntegerOption(intOption: Option[Int]): Option[Integer] = intOption.map(intValue => {
    val integerValue = intValue
    integerValue
  })

  def handleParameters(diffGraphBuilder: DiffGraphBuilder, function: Function, methodNode: NewMethod): Unit = {
    if (function.isThunk)
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
          diffGraphBuilder.addEdge(methodNode, node, EdgeTypes.AST)
        }
    else {
      getHighFunction(function).foreach { highFunction =>
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
            diffGraphBuilder.addEdge(methodNode, node, EdgeTypes.AST)
          }
      }
    }
  }

  def handleLocals(diffGraphBuilder: DiffGraphBuilder, function: Function, blockNode: NewBlock): Unit = {
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

  def handleBody(
    diffGraphBuilder: DiffGraphBuilder,
    function: Function,
    methodNode: NewMethod,
    blockNode: NewBlock
  ): Unit = {
    val instructions = getInstructions(function)
    if (instructions.nonEmpty) {
      var prevInstructionNode = addCallOrReturnNode(instructions.head)
      handleArguments(diffGraphBuilder, instructions.head, prevInstructionNode, function)
      diffGraphBuilder.addEdge(blockNode, prevInstructionNode, EdgeTypes.AST)
      diffGraphBuilder.addEdge(methodNode, prevInstructionNode, EdgeTypes.CFG)
      instructions.drop(1).foreach { instruction =>
        val instructionNode = addCallOrReturnNode(instruction)
        diffGraphBuilder.addNode(instructionNode)
        handleArguments(diffGraphBuilder, instruction, instructionNode, function)
        diffGraphBuilder.addEdge(blockNode, instructionNode, EdgeTypes.AST)
        diffGraphBuilder.addEdge(prevInstructionNode, instructionNode, EdgeTypes.CFG)
        prevInstructionNode = instructionNode
      }
    }
  }

  // Iterating over operands and add edges to call
  def handleArguments(
    diffGraphBuilder: DiffGraphBuilder,
    instruction: Instruction,
    callNode: CfgNodeNew,
    function: Function
  ): Unit = {
    val mnemonicString = processor.getInstructions.getOrElse(instruction.getMnemonicString, "UNKNOWN")
    if (mnemonicString.equals("CALL")) {
      val calledFunction = codeUnitFormat.getOperandRepresentationString(instruction, 0)
      functionByName.get(calledFunction).map { callee =>
        // Array of tuples containing (checked parameter name, parameter index, parameter data type)
        var checkedParameters = Array.empty[(String, Int, String)]

        if (callee.isThunk) {
          // thunk functions contain parameters already
          val parameters = callee.getParameters
          // TODO:
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
            .toHighFunction(callee)
            .map { highFunction =>
              highFunction.getLocalSymbolMap.getSymbols.asScala.toSeq.filter(_.isParameter).toArray
            }
            .getOrElse(Array.empty[HighSymbol])

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
    } else
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
                  .code(s"0x${scalar}")
                  .order(index + 1)
                  .typeFullName(scalar)
                  .lineNumber(Some(instruction.getMinAddress.getOffsetAsBigInteger.intValue))
                connectCallToArgument(diffGraphBuilder, callNode, node)
              case "GenericAddress" =>
                // TODO: try to resolve the address
                val genericAddress =
                  opObject.asInstanceOf[GenericAddress].toString()
                val node = nodes
                  .NewLiteral()
                  .code(s"0x${genericAddress}")
                  .order(index + 1)
                  .typeFullName(genericAddress)
                  .lineNumber(Some(instruction.getMinAddress.getOffsetAsBigInteger.intValue))
                connectCallToArgument(diffGraphBuilder, callNode, node)
              case _ =>
                println(s"""Unsupported argument: $opObject $className""")
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

  def sanitizeMethodName(methodName: String): String =
    methodName.split(">").lastOption.getOrElse(methodName).replace("[", "").replace("]", "")

}
