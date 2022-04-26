package io.joern.ghidra2cpg.passes

import ghidra.program.model.listing.{CodeUnitFormat, CodeUnitFormatOptions, Function, Instruction, Program}
import ghidra.program.model.pcode.PcodeOp._
import ghidra.program.model.pcode.{HighFunction, PcodeOp, Varnode}
import io.joern.ghidra2cpg._
import io.joern.ghidra2cpg.processors._
import io.joern.ghidra2cpg.utils.Decompiler
import io.joern.ghidra2cpg.utils.Utils.{createCallNode, _}
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


  def resolveVarNode(instruction: Instruction, input: Varnode, index: Int): CfgNodeNew = {
    createIdentifier(
      "TODO",
      "TODO",
      index + 1,
      Types.registerType("TODO"),
      instruction.getMinAddress.getOffsetAsBigInteger.intValue
    )
  }

  /*if (input.isRegister) {
  // TODO:
  var name = ""
  if (
    input.getDef != null && input.getDef.getInputs != null
  ) {
    val symbol = input.getDef.getInputs.toList.lastOption
      .flatMap(x => Option(x.getHigh))
      .flatMap(x => Option(x.getSymbol))
    if (symbol.isDefined) name = symbol.get.getName
  }
  if (name == null) name = input.getHigh.getSymbol.getName
  createIdentifier(
    name,
    name,
    index + 1,
    Types.registerType(name),
    instruction.getMinAddress.getOffsetAsBigInteger.intValue
  )
} else if (input.isConstant)
  createLiteral(
    "0x" + input.getWordOffset.toHexString,
    index + 1,
    index + 1,
    "0x" + input.getWordOffset.toHexString,
    instruction.getMinAddress.getOffsetAsBigInteger.intValue
  )
else if (input.isUnique) {
  var valueString = ""
  if (input.getDescendants.asScala.toList.head.getOutput == null)
    valueString = input.getDef.getInputs.toList.head.getAddress.getOffset.toString
  else valueString = input.getDescendants.asScala.toList.head.getOutput.getHigh.getName

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
}*/


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

  def handleAssignment1(
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
        if (pcodeOp.getInput(0).getDef != null)
          resolveArgument(diffGraphBuilder, instruction, callNode, pcodeOp.getInput(0).getDef, index)
      case PTRSUB | PTRADD => handlePtrSub(diffGraphBuilder, instruction, callNode, pcodeOp.getOutput, index)
      case _ => // handleDefault(pcodeAst)
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
      decompiler.toHighFunction(function).get.getLocalSymbolMap.getSymbols.asScala.toSeq
        .filter(_.isParameter)
        .foreach { parameter =>
          val checkedParameterName = Option(parameter.getStorage)
            .flatMap(x => Option(x.getRegister))
            .flatMap(x => Option(x.getName))
            .getOrElse(parameter.getName)
          val node =
            createParameterNode(
              checkedParameterName,
              checkedParameterName,
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

  def handleStore(pcodeOps: List[PcodeOp]): Unit = {
    //pcodeOps.last.getInputs.zipWithIndex.foreach{case(varNode,index)=>resolveVarNode(varnode, index))}
    println("todo handleStore")
  }

  def handleInstruction(diffGraphBuilder: DiffGraphBuilder, instruction: Instruction): CfgNodeNew = {
    val pcodeOp = instruction.getPcode().toList.last
    if (pcodeOp != null) {
      pcodeOp.getOpcode match {
        case INT_EQUAL | INT_NOTEQUAL | INT_SLESS | INT_SLESSEQUAL | INT_LESS | INT_LESSEQUAL =>
          println("TODO: INT_EQUAL | INT_NOTEQUAL | INT_SLESS | INT_SLESSEQUAL | INT_LESS | INT_LESSEQUAL ")
          createCallNode(instruction.toString, instruction.toString, instruction.getMinAddress.getOffsetAsBigInteger.intValue)
        case COPY =>
          val callNode = createCallNode("<operator>.assignment", instruction.toString, instruction.getMinAddress.getOffsetAsBigInteger.intValue)
          handleAssignment1(diffGraphBuilder, instruction, callNode, pcodeOp.getOutput, 0)
          callNode
        case LOAD =>
          val callNode = createCallNode("<operator>.assignment", instruction.toString, instruction.getMinAddress.getOffsetAsBigInteger.intValue)
          handleAssignment1(diffGraphBuilder, instruction, callNode, pcodeOp.getOutput, 0)
          callNode
        case SUBPIECE =>
          val callNode = createCallNode("<operator>.assignment", instruction.toString, instruction.getMinAddress.getOffsetAsBigInteger.intValue)
          handleAssignment1(diffGraphBuilder, instruction, callNode, pcodeOp.getOutput, 0)
          callNode
        case STORE => handleStore(instruction.getPcode.toList)
          val callNode = createCallNode("<operator>.assignment", instruction.toString, instruction.getMinAddress.getOffsetAsBigInteger.intValue)
          handleAssignment1(diffGraphBuilder, instruction, callNode, pcodeOp.getOutput, 0)
          callNode
        case CALL => println("TODO: CALL")
          createCallNode("UNKNOWN", "UNKNOWN", instruction.getMinAddress.getOffsetAsBigInteger.intValue)
        case CALLIND => println("TODO: CALLIND")
          createCallNode("UNKNOWN", "UNKNOWN", instruction.getMinAddress.getOffsetAsBigInteger.intValue)
        case INT_ADD | FLOAT_ADD =>
          handleTwoArguments(diffGraphBuilder, instruction, opNode, pcodeOp.last, "+", "<operator>.addition")
          createCallNode("UNKNOWN", "UNKNOWN", instruction.getMinAddress.getOffsetAsBigInteger.intValue)
        case INT_DIV | FLOAT_DIV | INT_SDIV =>
          //handleTwoArguments(diffGraphBuilder, instruction, opNode, pcodeOp.last, "/", "<operator>.division")
          createCallNode("UNKNOWN", "UNKNOWN", instruction.getMinAddress.getOffsetAsBigInteger.intValue)
        case INT_SUB | FLOAT_SUB =>
          //handleTwoArguments(diffGraphBuilder, instruction, opNode, pcodeOp.last, "-", "<operator>.subtraction")
          createCallNode("UNKNOWN", "UNKNOWN", instruction.getMinAddress.getOffsetAsBigInteger.intValue)
        case INT_MULT | FLOAT_MULT =>
          //handleTwoArguments(diffGraphBuilder, instruction, opNode, pcodeOp.last, "*", "<operator>.multiplication")
          createCallNode("UNKNOWN", "UNKNOWN", instruction.getMinAddress.getOffsetAsBigInteger.intValue)
        case MULTIEQUAL | INDIRECT | PIECE => // not handled
          createCallNode("UNKNOWN", "UNKNOWN", instruction.getMinAddress.getOffsetAsBigInteger.intValue)
        case INT_XOR => println("TODO: ^")
          createCallNode("UNKNOWN", "UNKNOWN", instruction.getMinAddress.getOffsetAsBigInteger.intValue)
        //handleTwoArguments(diffGraphBuilder, instruction, opNode, pcodeOp.last, "^", "<operator>.xor")
        case INT_OR => println("TODO: |")
          createCallNode("UNKNOWN", "UNKNOWN", instruction.getMinAddress.getOffsetAsBigInteger.intValue)
        //handleTwoArguments(diffGraphBuilder, instruction, opNode, pcodeOp.last, "|", "<operator>.or")
        case CAST => println("TODO: CAST")
          createCallNode("UNKNOWN", "UNKNOWN", instruction.getMinAddress.getOffsetAsBigInteger.intValue)
        // we need to "unpack" the def of the first input of the cast
        // eg. "(param_1 + 5)" in "(void *)(param_1 + 5)"
        //if (pcodeOp.getInput(0).getDef != null)
        //  resolveArgument(diffGraphBuilder, instruction, opNode, pcodeOps.last.getInput(0).getDef, index)
        case PTRSUB | PTRADD =>
          //handlePtrSub(diffGraphBuilder, instruction, callNode, pcodeOp.getOutput, index)
          createCallNode("UNKNOWN", "UNKNOWN", instruction.getMinAddress.getOffsetAsBigInteger.intValue)
        case _ => createCallNode("UNKNOWN", "UNKNOWN", instruction.getMinAddress.getOffsetAsBigInteger.intValue)
      }
    } else {
      createCallNode("UNKNOWN", "UNKNOWN", instruction.getMinAddress.getOffsetAsBigInteger.intValue)
    }
  }



  def handleBody(
                  diffGraphBuilder: DiffGraphBuilder,
                  function: Function,
                  methodNode: NewMethod,
                  blockNode: NewBlock
                ): Unit = {
    // get asm instructions
    val instructionNodes = currentProgram.getListing
      .getInstructions(function.getBody, true)
      .iterator()
      .asScala
      .toList
      .map(instruction => handleInstruction(diffGraphBuilder, instruction ))

    if(instructionNodes.nonEmpty) {
      diffGraphBuilder.addEdge(blockNode, instructionNodes.head, EdgeTypes.AST)
      diffGraphBuilder.addEdge(methodNode, instructionNodes.head, EdgeTypes.CFG)
      instructionNodes.sliding(2).foreach { nodes =>
        val prevInstructionNode = nodes.head
        val instructionNode = nodes.last
        diffGraphBuilder.addEdge(blockNode, prevInstructionNode, EdgeTypes.AST)
        diffGraphBuilder.addEdge(methodNode, prevInstructionNode, EdgeTypes.CFG)
        diffGraphBuilder.addNode(instructionNode)
        diffGraphBuilder.addEdge(blockNode, instructionNode, EdgeTypes.AST)
      }
    }
  }

  override def runOnPart(diffGraphBuilder: DiffGraphBuilder, function: Function): Unit = {
    val localDiffGraph = new DiffGraphBuilder
    // we need it just once with default settings
    val blockNode = nodes.NewBlock().code("").order(0)
    val methodNode = createMethodNode(
      decompiler,
      function,
      fileName,
      checkIfExternal(currentProgram, function.getName)
    )
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
