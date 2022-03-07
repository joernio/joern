package io.joern.ghidra2cpg.passes.mips

import ghidra.program.model.address.GenericAddress
import ghidra.program.model.lang.Register
import ghidra.program.model.listing.{CodeUnitFormat, CodeUnitFormatOptions, Function, FunctionIterator, Instruction, Listing, Program}
import ghidra.program.model.pcode.PcodeOp._
import ghidra.program.model.pcode.{HighFunction, PcodeOp, Varnode}
import ghidra.program.model.scalar.Scalar
import io.joern.ghidra2cpg.processors.MipsProcessor
import io.joern.ghidra2cpg.utils.Nodes._
import io.joern.ghidra2cpg.{Decompiler, Types}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNodeNew, Method, NewBlock, NewMethod}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}
import io.shiftleft.passes.ConcurrentWriterCpgPass
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters._
import scala.language.implicitConversions

class MipsFunctionPass(
                        currentProgram: Program,
                        address2Literal: Map[Long, String],
                        filename: String,
                        function: Function,
                        cpg: Cpg,
                        decompiler: Decompiler
                      ) extends ConcurrentWriterCpgPass[Method](cpg) {

  val processor = new MipsProcessor()
  // val diffGraph: DiffGraphBuilder
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
  //extends FunctionPass(new MipsProcessor, currentProgram, function, cpg, decompiler) {
  private val logger = LoggerFactory.getLogger(classOf[MipsFunctionPass])
  protected var methodNode: Option[NewMethod] = None

  def resolveVarNode(instruction: Instruction, input: Varnode, index: Int): CfgNodeNew = {
    if (input.isRegister) {
      var name = input.getHigh.getName
      val high = input.getHigh
      if (
        high != null && input.getDef != null && high.getName == "UNNAMED" && input.getDef != null && input.getDef.getInputs != null
      ) {
        val symbol = input.getDef.getInputs.toList.lastOption
          .flatMap(x => Option(x.getHigh))
          .flatMap(x => Option(x.getSymbol))
        if (symbol.isDefined) {
          name = symbol.get.getName
        }
      }
      if (name == null) {
        name = input.getHigh.getSymbol.getName
      }
      createIdentifier(
        name,
        name,
        index + 1,
        Types.registerType(name),
        instruction.getMinAddress.getOffsetAsBigInteger.intValue
      )
    } else if (input.isConstant) {
      createLiteral(
        "0x" + input.getWordOffset.toHexString,
        index + 1,
        index + 1,
        "0x" + input.getWordOffset.toHexString,
        instruction.getMinAddress.getOffsetAsBigInteger.intValue
      )
    } else if (input.isUnique) {
      var valueString = ""
      if (input.getDescendants.asScala.toList.head.getOutput == null) {
        valueString = input.getDef.getInputs.toList.head.getAddress.getOffset.toString
      } else {
        valueString = input.getDescendants.asScala.toList.head.getOutput.getHigh.getName
      }

      val value = address2Literal.getOrElse(input.getDef.getInputs.toList.head.getAddress.getOffset, valueString)

      createLiteral(
        value,
        index + 1,
        index + 1,
        input.getWordOffset.toHexString,
        instruction.getMinAddress.getOffsetAsBigInteger.intValue
      )
    } else {
      // we default to literal
      // identifier could be useful too
      createLiteral(
        input.toString(),
        index + 1,
        index + 1,
        input.toString(),
        instruction.getMinAddress.getOffsetAsBigInteger.intValue
      )
    }
  }

  def handleAssignment(
                        diffGraphBuilder: DiffGraphBuilder,
                        instruction: Instruction,
                        callNode: CfgNodeNew,
                        to: Varnode,
                        index: Int
                      ): Unit = {
    val node = resolveVarNode(instruction, to, index)
    connectCallToArgument(diffGraphBuilder, callNode, node)
  }

  def handleTwoArguments(
                          diffGraphBuilder: DiffGraphBuilder,
                          instruction: Instruction,
                          callNode: CfgNodeNew,
                          pcodeOp: PcodeOp,
                          operand: String,
                          name: String
                        ): Unit = {
    val firstOp = resolveVarNode(instruction, pcodeOp.getInput(0), 1)
    val secondOp = resolveVarNode(instruction, pcodeOp.getInput(1), 2)
    val code = s"${firstOp.code} $operand ${secondOp.code}"
    val opNode = createCallNode(code = code, name, instruction.getMinAddress.getOffsetAsBigInteger.intValue)

    connectCallToArgument(diffGraphBuilder, opNode, firstOp)
    connectCallToArgument(diffGraphBuilder, opNode, secondOp)
    connectCallToArgument(diffGraphBuilder, callNode, opNode)
  }

  def handlePtrSub(
                    diffGraphBuilder: DiffGraphBuilder,
                    instruction: Instruction,
                    callNode: CfgNodeNew,
                    varNode: Varnode,
                    index: Int
                  ): Unit = {
    val arg = resolveVarNode(instruction, varNode, index)
    connectCallToArgument(diffGraphBuilder, callNode, arg)
  }

  def handleDefault(varNode: PcodeOp): Unit = {
    println("Unsupported " + varNode.toString + " " + varNode.getOpcode)
  }

  def resolveArgument(
                       diffGraphBuilder: DiffGraphBuilder,
                       instruction: Instruction,
                       callNode: CfgNodeNew,
                       pcodeAst: PcodeOp,
                       index: Int
                     ): Unit = {
    pcodeAst.getOpcode match {
      case INT_EQUAL | INT_NOTEQUAL | INT_SLESS | INT_SLESSEQUAL | INT_LESS | INT_LESSEQUAL =>
        logger.warn("INT_EQUAL | INT_NOTEQUAL | INT_SLESS | INT_SLESSEQUAL | INT_LESS | INT_LESSEQUAL ")
      case CALL | CALLIND =>
        handleAssignment(diffGraphBuilder, instruction, callNode, pcodeAst.getOutput, index)
      case INT_ADD | FLOAT_ADD =>
        handleTwoArguments(diffGraphBuilder, instruction, callNode, pcodeAst, "+", "<operator>.addition")
      case INT_DIV | FLOAT_DIV | INT_SDIV =>
        handleTwoArguments(diffGraphBuilder, instruction, callNode, pcodeAst, "/", "<operator>.division")
      case INT_SUB | FLOAT_SUB =>
        handleTwoArguments(diffGraphBuilder, instruction, callNode, pcodeAst, "-", "<operator>.subtraction")
      case INT_MULT | FLOAT_MULT =>
        handleTwoArguments(diffGraphBuilder, instruction, callNode, pcodeAst, "*", "<operator>.multiplication")
      case MULTIEQUAL | INDIRECT | PIECE => // not handled
      case INT_XOR =>
        handleTwoArguments(diffGraphBuilder, instruction, callNode, pcodeAst, "^", "<operator>.xor")
      case INT_OR =>
        handleTwoArguments(diffGraphBuilder, instruction, callNode, pcodeAst, "^", "<operator>.xor")
      case COPY | LOAD | STORE | SUBPIECE =>
        handleAssignment(diffGraphBuilder, instruction, callNode, pcodeAst.getOutput, index)
      case CAST =>
        // we need to "unpack" the def of the first input of the cast
        // eg. "(param_1 + 5)" in "(void *)(param_1 + 5)"
        if (pcodeAst.getInput(0).getDef != null) {
          resolveArgument(diffGraphBuilder, instruction, callNode, pcodeAst.getInput(0).getDef, index)
        }
      case PTRSUB | PTRADD => handlePtrSub(diffGraphBuilder, instruction, callNode, pcodeAst.getOutput, index)
      case _ => // handleDefault(pcodeAst)
    }
  }

  def addCallArguments(diffGraphBuilder: DiffGraphBuilder, instruction: Instruction, callNode: CfgNodeNew): Unit = {
    val opCodes = highFunction
      .getPcodeOps(instruction.getAddress())
      .asScala
      .toList
    if (opCodes.size < 2) {
      return
    }
    // first input is the address to the called function
    // we know it already
    val arguments = opCodes.head.getInputs.toList.drop(1)
    arguments.zipWithIndex.foreach { case (value, index) =>
      if (value.getDef != null)
        resolveArgument(diffGraphBuilder, instruction, callNode, value.getDef, index)
    }
  }

  def addInstructionArguments(
                               diffGraphBuilder: DiffGraphBuilder,
                               instruction: Instruction,
                               instructionNode: CfgNodeNew
                             ): Unit = {
    for (index <- 0 until instruction.getNumOperands) {
      val opObjects = instruction.getOpObjects(index)
      for (opObject <- opObjects) {
        opObject match {
          case register: Register =>
            val node = createIdentifier(
              register.getName,
              register.getName,
              index + 1,
              Types.registerType(register.getName),
              instruction.getMinAddress.getOffsetAsBigInteger.intValue
            )
            connectCallToArgument(diffGraphBuilder, instructionNode, node)
          case scalar: Scalar =>
            val node = createLiteral(
              scalar.toString(16, false, false, "", ""),
              index + 1,
              index + 1,
              scalar.toString(16, false, false, "", ""),
              instruction.getMinAddress.getOffsetAsBigInteger.intValue
            )
            connectCallToArgument(diffGraphBuilder, instructionNode, node)
          case genericAddress: GenericAddress =>
            val node = createLiteral(
              genericAddress.toString,
              index + 1,
              index + 1,
              genericAddress.toString,
              instruction.getMinAddress.getOffsetAsBigInteger.intValue
            )
            connectCallToArgument(diffGraphBuilder, instructionNode, node)
          case _ =>
            println(s"""Unsupported argument: $opObject ${opObject.getClass.getSimpleName}""")
        }
      }
    }
  }

  // Iterating over operands and add edges to call
  def handleArguments(
                       diffGraphBuilder: DiffGraphBuilder,
                       instruction: Instruction,
                       callNode: CfgNodeNew
                     ): Unit = {
    if (instruction.getPcode.toList.isEmpty) {
      // nop && _nop
      return
    }
    // CALL is the last PcodeOp
    val opCodes = instruction.getPcode.toList // .last.getOpcode
    opCodes.last.getOpcode match {
      case CALLIND | CALL =>
        addCallArguments(diffGraphBuilder, instruction, callNode)
      case _ =>
        // regular instructions, eg. add/sub
        addInstructionArguments(diffGraphBuilder, instruction, callNode)
    }
  }

  override def runOnPart(diffGraphBuilder: DiffGraphBuilder, method: Method): Unit = {
    try {
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
    } catch {
      case e: Exception =>
        e.printStackTrace()
        println(e.getMessage)
    }
  }

  // override def partIterator: Iterator[Method] = cpg.method.l.iterator

  implicit def intToIntegerOption(intOption: Option[Int]): Option[Integer] = {
    intOption.map(intValue => {
      val integerValue: Integer = intValue
      integerValue
    })
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
        diffGraphBuilder.addEdge(prevInstructionNode, instructionNode, EdgeTypes.CFG)
        prevInstructionNode = instructionNode
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
