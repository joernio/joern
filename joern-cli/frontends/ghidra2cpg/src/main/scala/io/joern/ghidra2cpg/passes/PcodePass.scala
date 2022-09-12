package io.joern.ghidra2cpg.passes

import ghidra.program.model.listing.{Function, Instruction, Program}
import ghidra.program.model.pcode.{HighFunction, PcodeOp, Varnode}
import io.joern.ghidra2cpg._
import io.joern.ghidra2cpg.utils.Utils._
import io.joern.ghidra2cpg.utils.{Decompiler, PCodeMapper}
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
  decompiler: Decompiler
) extends ConcurrentWriterCpgPass[Function](cpg) {

  def getHighFunction(function: Function): HighFunction = decompiler.toHighFunction(function).orNull

  def resolveVarNode(instruction: Instruction, input: Varnode, index: Int): CfgNodeNew = {
    if (input.isRegister) {
      createIdentifier(
        currentProgram.getRegister(input).getName,
        currentProgram.getRegister(input).getName,
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
      if (input.getDescendants != null) {

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
        createIdentifier(
          "TODO",
          "TODO",
          index + 1,
          Types.registerType("TODO"),
          instruction.getMinAddress.getOffsetAsBigInteger.intValue
        )
      }
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

  def handleStore(
    diffGraphBuilder: DiffGraphBuilder,
    instruction: Instruction,
    callNode: CfgNodeNew,
    pcodeOp: PcodeOp
  ): Unit = {
    val firstOp  = resolveVarNode(instruction, pcodeOp.getInput(1), 2)
    val secondOp = resolveVarNode(instruction, pcodeOp.getInput(2), 1)
    connectCallToArgument(diffGraphBuilder, callNode, firstOp)
    connectCallToArgument(diffGraphBuilder, callNode, secondOp)
  }
  def handleAssignment(
    diffGraphBuilder: DiffGraphBuilder,
    instruction: Instruction,
    callNode: CfgNodeNew,
    pcodeOp: PcodeOp
  ): Unit = {
    val firstOp  = resolveVarNode(instruction, pcodeOp.getOutput, 2)
    val secondOp = resolveVarNode(instruction, pcodeOp.getInput(0), 1)
    connectCallToArgument(diffGraphBuilder, callNode, firstOp)
    connectCallToArgument(diffGraphBuilder, callNode, secondOp)
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
    // connectCallToArgument(diffGraphBuilder, callNode, opNode)
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
      decompiler
        .toHighFunction(function)
        .get
        .getLocalSymbolMap
        .getSymbols
        .asScala
        .toSeq
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
        createIdentifier(local.getName, local.getSymbol.getName, 1000, local.getDataType.toString, 1000)

      diffGraphBuilder.addNode(localNode)
      diffGraphBuilder.addNode(identifier)
      diffGraphBuilder.addEdge(blockNode, localNode, EdgeTypes.AST)
      diffGraphBuilder.addEdge(blockNode, identifier, EdgeTypes.AST)
      diffGraphBuilder.addEdge(identifier, localNode, EdgeTypes.REF)
    }
  }

  def handleInstruction(
    diffGraphBuilder: DiffGraphBuilder,
    instruction: Instruction,
    function: Function
  ): CfgNodeNew = {
    new PCodeMapper(
      diffGraphBuilder,
      instruction,
      functions,
      decompiler,
      getHighFunction(function),
      address2Literal
    ).getCallNode
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
      .map { instruction =>
        handleInstruction(diffGraphBuilder, instruction, function: Function)
      }
    instructionNodes.foreach(diffGraphBuilder.addNode)
    if (instructionNodes.nonEmpty) {
      diffGraphBuilder.addEdge(blockNode, instructionNodes.head, EdgeTypes.AST)
      diffGraphBuilder.addEdge(methodNode, instructionNodes.head, EdgeTypes.CFG)
      instructionNodes.sliding(2).foreach { nodes =>
        val prevInstructionNode = nodes.head
        val instructionNode     = nodes.last
        println(instructionNode.code)
        diffGraphBuilder.addEdge(blockNode, instructionNode, EdgeTypes.AST)
        diffGraphBuilder.addEdge(prevInstructionNode, instructionNode, EdgeTypes.CFG)
      }
    }
  }

  override def runOnPart(diffGraphBuilder: DiffGraphBuilder, function: Function): Unit = {
    val localDiffGraph = new DiffGraphBuilder
    // we need it just once with default settings
    val blockNode  = nodes.NewBlock().code("").order(0)
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
