package io.joern.ghidra2cpg.passes

import ghidra.program.model.address.GenericAddress
import ghidra.program.model.lang.Register
import ghidra.program.model.listing.{CodeUnitFormat, CodeUnitFormatOptions, Function, Instruction, Program}
import ghidra.program.model.pcode.PcodeOp._
import ghidra.program.model.pcode.{HighFunction, PcodeOp, PcodeOpAST, Varnode}
import ghidra.program.model.scalar.Scalar
import io.joern.ghidra2cpg._
import io.joern.ghidra2cpg.processors._
import io.joern.ghidra2cpg.utils.Decompiler
import io.joern.ghidra2cpg.utils.Nodes._
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNodeNew, NewBlock, NewMethod}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}
import io.shiftleft.passes.ConcurrentWriterCpgPass
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters._
import scala.language.implicitConversions

class PcodePass(
  currentProgram: Program,
  address2Literal: Map[Long, String],
  fileName: String,
  functions: List[Function],
  cpg: Cpg,
  decompiler: Decompiler,
  processor: Processor
) extends ConcurrentWriterCpgPass[Function](cpg) {

  private val logger = LoggerFactory.getLogger(classOf[PcodePass])

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
      true
    )
  )

  def getHighFunction(function: Function): HighFunction = decompiler.toHighFunction(function).orNull

  def connectCallToArgument(diffGraphBuilder: DiffGraphBuilder, call: CfgNodeNew, argument: CfgNodeNew): Unit = {
    diffGraphBuilder.addNode(argument)
    diffGraphBuilder.addEdge(call, argument, EdgeTypes.ARGUMENT)
    diffGraphBuilder.addEdge(call, argument, EdgeTypes.AST)
  }

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
    val firstOp  = resolveVarNode(instruction, pcodeOp.getInput(0), 1)
    val secondOp = resolveVarNode(instruction, pcodeOp.getInput(1), 2)
    val code     = s"${firstOp.code} $operand ${secondOp.code}"
    val opNode   = createCallNode(code = code, name, instruction.getMinAddress.getOffsetAsBigInteger.intValue)

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
    pcodeOp: PcodeOp,
    index: Int
  ): Unit = {
    pcodeOp.getOpcode match {
      case INT_EQUAL | INT_NOTEQUAL | INT_SLESS | INT_SLESSEQUAL | INT_LESS | INT_LESSEQUAL =>
        logger.warn("INT_EQUAL | INT_NOTEQUAL | INT_SLESS | INT_SLESSEQUAL | INT_LESS | INT_LESSEQUAL ")
      case CALL | CALLIND =>
        handleAssignment(diffGraphBuilder, instruction, callNode, pcodeOp.getOutput, index)
      case INT_ADD | FLOAT_ADD =>
        handleTwoArguments(diffGraphBuilder, instruction, callNode, pcodeOp, "+", "<operator>.addition")
      case INT_DIV | FLOAT_DIV | INT_SDIV =>
        handleTwoArguments(diffGraphBuilder, instruction, callNode, pcodeOp, "/", "<operator>.division")
      case INT_SUB | FLOAT_SUB =>
        handleTwoArguments(diffGraphBuilder, instruction, callNode, pcodeOp, "-", "<operator>.subtraction")
      case INT_MULT | FLOAT_MULT =>
        handleTwoArguments(diffGraphBuilder, instruction, callNode, pcodeOp, "*", "<operator>.multiplication")
      case MULTIEQUAL | INDIRECT | PIECE => // not handled
      case INT_XOR =>
        handleTwoArguments(diffGraphBuilder, instruction, callNode, pcodeOp, "^", "<operator>.xor")
      case INT_OR =>
        handleTwoArguments(diffGraphBuilder, instruction, callNode, pcodeOp, "^", "<operator>.xor")
      case COPY | LOAD | STORE | SUBPIECE =>
        handleAssignment(diffGraphBuilder, instruction, callNode, pcodeOp.getOutput, index)
      case CAST =>
        // we need to "unpack" the def of the first input of the cast
        // eg. "(param_1 + 5)" in "(void *)(param_1 + 5)"
        if (pcodeOp.getInput(0).getDef != null) {
          resolveArgument(diffGraphBuilder, instruction, callNode, pcodeOp.getInput(0).getDef, index)
        }
      case PTRSUB | PTRADD => handlePtrSub(diffGraphBuilder, instruction, callNode, pcodeOp.getOutput, index)
      case _               => // handleDefault(pcodeAst)
    }
  }

  def addCallArguments(
    diffGraphBuilder: DiffGraphBuilder,
    instruction: Instruction,
    callNode: CfgNodeNew,
    highFunction: HighFunction
  ): Unit = {
    val opCodes: Seq[PcodeOpAST] = highFunction
      .getPcodeOps(instruction.getAddress())
      .asScala
      .toList

    if (opCodes.size < 1) {
      return
    }
    // first input is the address to the called function
    // we know it already
    val arguments = opCodes.head.getInputs.toList.drop(1)
    arguments.zipWithIndex.foreach { case (value, index) =>
      if (value.getDef != null) {
        resolveArgument(diffGraphBuilder, instruction, callNode, value.getDef, index)
      }
      if (value.isConstant) {
        val argument = resolveVarNode(instruction, value, index)
        connectCallToArgument(diffGraphBuilder, callNode, argument)
      }
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
    callNode: CfgNodeNew,
    function: Function
  ): Unit = {
    if (instruction.getPcode.toList.isEmpty) {
      // nop && _nop
      return
    }
    // CALL is the last PcodeOp
    val opCodes: Seq[PcodeOp] = instruction.getPcode.toList

    opCodes.last.getOpcode match {
      case CALLIND | CALL =>
        val highFunction = getHighFunction(function)
        addCallArguments(diffGraphBuilder, instruction, callNode, highFunction)
      case _ =>
        // regular instructions, eg. add/sub
        addInstructionArguments(diffGraphBuilder, instruction, callNode)
    }
  }

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
    else
      getHighFunction(function).getLocalSymbolMap.getSymbols.asScala.toSeq
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

  def sanitizeMethodName(methodName: String): String =
    methodName.split(">").lastOption.getOrElse(methodName).replace("[", "").replace("]", "")

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

  protected def getInstructions(function: Function): Seq[Instruction] =
    currentProgram.getListing.getInstructions(function.getBody, true).iterator().asScala.toList

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
        // Not connecting the previous instruction, if it is an unconditional jump
        // TODO: JMP is x86 specific
        if (!prevInstructionNode.code.startsWith("JMP")) {
          diffGraphBuilder.addEdge(prevInstructionNode, instructionNode, EdgeTypes.CFG)
        }
        prevInstructionNode = instructionNode
      }
    }
  }

  override def runOnPart(diffGraphBuilder: DiffGraphBuilder, function: Function): Unit = {
    val localDiffGraph = new DiffGraphBuilder
    // we need it just once with default settings
    val blockNode: NewBlock = nodes.NewBlock().code("").order(0)
    val methodNode = createMethodNode(decompiler, function, fileName, checkIfExternal(currentProgram, function.getName))
    val methodReturn = createReturnNode()
    localDiffGraph.addNode(methodNode)
    localDiffGraph.addNode(blockNode)
    localDiffGraph.addEdge(methodNode, blockNode, EdgeTypes.AST)
    localDiffGraph.addNode(methodReturn)
    localDiffGraph.addEdge(methodNode, methodReturn, EdgeTypes.AST)
    handleParameters(diffGraphBuilder, function, methodNode)
    handleLocals(diffGraphBuilder, function, blockNode)
    handleBody(diffGraphBuilder, function, methodNode, blockNode)
    diffGraphBuilder.absorb(localDiffGraph)
  }

  override def generateParts(): Array[Function] = functions.toArray
}
