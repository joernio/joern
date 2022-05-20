package io.joern.ghidra2cpg.passes

import ghidra.program.model.listing.{CodeUnitFormat, CodeUnitFormatOptions, Function, Instruction, Program}
import ghidra.program.model.pcode.PcodeOp._
import ghidra.program.model.pcode.{HighFunction, PcodeOp, PcodeOpAST, Varnode}
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
    var returnNode: CfgNodeNew = createIdentifier(
      "TODO",
      "TODO",
      index + 1,
      Types.registerType("TODO"),
      instruction.getMinAddress.getOffsetAsBigInteger.intValue
    )
    if (input.isRegister) {
      returnNode = createIdentifier(
        currentProgram.getRegister(input).getName,
        currentProgram.getRegister(input).getName,
        index + 1,
        Types.registerType(name),
        instruction.getMinAddress.getOffsetAsBigInteger.intValue
      )
    } else if (input.isConstant)
      returnNode = createLiteral(
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

        returnNode = createLiteral(
          value,
          index + 1,
          index + 1,
          input.getWordOffset.toHexString,
          instruction.getMinAddress.getOffsetAsBigInteger.intValue
        )
      }
    } else {
      // we default to literal
      // identifier could be useful too
      returnNode = createLiteral(
        input.toString(),
        index + 1,
        index + 1,
        input.toString(),
        instruction.getMinAddress.getOffsetAsBigInteger.intValue
      )
    }
    returnNode
  }

  // def handleAssignment(
  //                      diffGraphBuilder: DiffGraphBuilder,
  //                      instruction: Instruction,
  //                      callNode: CfgNodeNew,
  //                      to: Varnode,
  //                      index: Int
  //                    ): Unit = {
  //  val node = resolveVarNode(instruction, to, index)
  //  connectCallToArgument(diffGraphBuilder, callNode, node)
  // }

  // def handleStore(
  //  diffGraphBuilder: DiffGraphBuilder,
  //  instruction: Instruction,
  //  callNode: CfgNodeNew,
  //  pcodeOp: PcodeOp
  // ): Unit = {
  //  val firstOp  = resolveVarNode(instruction, pcodeOp.getInput(1), 2)
  //  val secondOp = resolveVarNode(instruction, pcodeOp.getInput(2), 1)
  //  connectCallToArgument(diffGraphBuilder, callNode, firstOp)
  //  connectCallToArgument(diffGraphBuilder, callNode, secondOp)
  // }
  // def handleAssignment(
  //  diffGraphBuilder: DiffGraphBuilder,
  //  instruction: Instruction,
  //  callNode: CfgNodeNew,
  //  pcodeOp: PcodeOp
  // ): Unit = {
  //  val firstOp  = resolveVarNode(instruction, pcodeOp.getOutput, 2)
  //  val secondOp = resolveVarNode(instruction, pcodeOp.getInput(0), 1)
  //  connectCallToArgument(diffGraphBuilder, callNode, firstOp)
  //  connectCallToArgument(diffGraphBuilder, callNode, secondOp)
  // }

  // def handleTwoArguments(
  //  diffGraphBuilder: DiffGraphBuilder,
  //  instruction: Instruction,
  //  callNode: CfgNodeNew,
  //  pcodeOp: PcodeOp,
  //  operand: String,
  //  name: String
  // ): Unit = {
  //  val firstOp  = resolveVarNode(instruction, pcodeOp.getInput(0), 1)
  //  val secondOp = resolveVarNode(instruction, pcodeOp.getInput(1), 2)
  //  val code     = s"${firstOp.code} $operand ${secondOp.code}"
  //  val opNode   = createCallNode(code = code, name, instruction.getMinAddress.getOffsetAsBigInteger.intValue)

  //  connectCallToArgument(diffGraphBuilder, opNode, firstOp)
  //  connectCallToArgument(diffGraphBuilder, opNode, secondOp)
  //  // connectCallToArgument(diffGraphBuilder, callNode, opNode)
  // }

  def handleTwoArguments(
    diffGraphBuilder: DiffGraphBuilder,
    nativeInstruction: Instruction,
    pcodeOp: PcodeOp,
    cpgOperationName: String,
    operation: String
  ): CfgNodeNew = {
    val firstOp  = resolveVarNode(nativeInstruction, pcodeOp.getInput(0), 1)
    val secondOp = resolveVarNode(nativeInstruction, pcodeOp.getInput(1), 2)
    val callNode = createCall(
      cpgOperationName,
      s"${firstOp.code} $operation ${secondOp.code}",
      nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue
    )
    connectCallToArgument(diffGraphBuilder, callNode, firstOp)
    connectCallToArgument(diffGraphBuilder, callNode, secondOp)
    callNode
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
        createIdentifier(local.getName, local.getSymbol.getName, -1, local.getDataType.toString, -1)

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
  def createCall(name: String, code: String, address: Int): CfgNodeNew = {
    createCallNode(code, name, address)
  }

  def handleSingleArgument(
    diffGraphBuilder: DiffGraphBuilder,
    nativeInstruction: Instruction,
    pcodeOp: PcodeOp,
    cpgOperationName: String,
    operation: String,
    address: Int
  ): CfgNodeNew = {
    val firstOp  = resolveVarNode(nativeInstruction, pcodeOp.getInput(0), 1)
    val callNode = createCall(cpgOperationName, s"$operation(${firstOp.code})", address)
    connectCallToArgument(diffGraphBuilder, callNode, firstOp)
    callNode
  }

  def handleStore(diffGraphBuilder: DiffGraphBuilder, nativeInstruction: Instruction, pcodeOp: PcodeOp): CfgNodeNew = {
    val output   = resolveVarNode(nativeInstruction, pcodeOp.getInputs.headOption.get, 0)
    val secondOp = resolveVarNode(nativeInstruction, pcodeOp.getInputs.headOption.get, 1)
    val callNode = createCall(
      "<operator>.assignment",
      nativeInstruction.toString,
      nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue()
    )
    connectCallToArgument(diffGraphBuilder, callNode, output)
    connectCallToArgument(diffGraphBuilder, callNode, secondOp)
    callNode
  }
  def handleAssignment(
    diffGraphBuilder: DiffGraphBuilder,
    nativeInstruction: Instruction,
    pcodeOp: PcodeOp
  ): CfgNodeNew = {
    val firstOp = createCallNode(
      pcodeOp.getOutput.toString,
      pcodeOp.getOutput.toString,
      pcodeOp.getSeqnum.getTarget.getOffsetAsBigInteger.intValue()
    ) // , 0)
    val secondOp = resolveVarNode(nativeInstruction, pcodeOp.getInputs.headOption.get, 1)
    val callNode = createCall(
      "<operator>.assignment",
      s"${firstOp.code} = ${secondOp.code}",
      nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue()
    )
    connectCallToArgument(diffGraphBuilder, callNode, firstOp)
    connectCallToArgument(diffGraphBuilder, callNode, secondOp)
    callNode
  }
  def mapCallNode(
    diffGraphBuilder: DiffGraphBuilder,
    nativeInstruction: Instruction,
    pcodeOpAst: PcodeOpAST,
    address: Int,
    highFunction: HighFunction
  ): CfgNodeNew = {
    // var callNode: CfgNodeNew = createCallNode("UNKNOWN", "UNKNOWN", -1)
    val callNode = pcodeOpAst.getOpcode match {
      // TODO add more pcode ops like CALL.*
      case BRANCH | BRANCHIND | CBRANCH =>
        createCallNode(
          nativeInstruction.toString,
          "<operator>.goto",
          nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue
        )
      case RETURN =>
        createCall("TODO RET", "TODO RET", address)
      case CALL | CALLOTHER | CALLIND =>
        val calledFunction = codeUnitFormat
          .getOperandRepresentationString(nativeInstruction, 0)
          .split(">")
          .last
          .replace("[", "")
          .replace("]", "")
        val callee = functions.filter(_.getName == calledFunction)
        val _callNode =
          createCallNode(calledFunction, calledFunction, address)
        // val opCodes: Seq[PcodeOpAST] = highFunction
        //  .getPcodeOps(nativeInstruction.getAddress())
        //  .asScala
        //  .toList
        //// if (opCodes.size < 2) {
        ////  return
        //// }
        //// first input is the address to the called function
        //// we know it already
        //// val arguments = opCodes.head.getInputs.toList.drop(1)
        //// arguments.zipWithIndex.foreach { case (value, index) =>
        ////  if (value.getDef != null)
        ////    //println("ARGGGGGGGGGGGGGGGGGGGG " + arguments.mkString(" ::: ") +" -> "+
        ////      connectCallToArgument(_callNode, resolveVarNode(value, index))
        //// }
        // val parameters = decompiler
        //  .toHighFunction(callee.head)
        //  .get
        //  .getLocalSymbolMap
        //  .getSymbols
        //  .asScala
        //  .toSeq
        //  .filter(_.isParameter)
        //  .toArray

        // parameters.zipWithIndex.foreach { case (parameter, index) =>
        //  val node = createIdentifier(
        //    parameter.getName,
        //    // parameter,
        //    parameter.getName,
        //    index + 1,
        //    Types.registerType(parameter.getDataType.getName),
        //    nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue
        //  )
        //  connectCallToArgument(diffGraphBuilder, _callNode, node)
        // }
        _callNode
      case BOOL_AND =>
        handleTwoArguments(diffGraphBuilder, nativeInstruction, pcodeOpAst, "<operator>.and", "&&")
      case BOOL_NEGATE =>
        handleSingleArgument(
          diffGraphBuilder,
          nativeInstruction,
          pcodeOpAst,
          "<operator>.negate",
          pcodeOpAst.getMnemonic,
          address
        )
      case BOOL_OR =>
        handleTwoArguments(diffGraphBuilder, nativeInstruction, pcodeOpAst, "<operator>.or", "||")
      case BOOL_XOR =>
        handleTwoArguments(diffGraphBuilder, nativeInstruction, pcodeOpAst, "<operator>.xor", "^^")

      case CAST =>
        handleSingleArgument(
          diffGraphBuilder,
          nativeInstruction,
          pcodeOpAst,
          "<operator>.cast",
          pcodeOpAst.getMnemonic,
          address
        )
      case CPOOLREF =>
        handleSingleArgument(
          diffGraphBuilder,
          nativeInstruction,
          pcodeOpAst,
          "<operator>.cpoolref",
          pcodeOpAst.getMnemonic,
          address
        )
      case EXTRACT =>
        handleSingleArgument(
          diffGraphBuilder,
          nativeInstruction,
          pcodeOpAst,
          "<operator>.extract",
          pcodeOpAst.getMnemonic,
          address
        )
      case FLOAT_ABS =>
        handleSingleArgument(
          diffGraphBuilder,
          nativeInstruction,
          pcodeOpAst,
          "<operator>.abs",
          pcodeOpAst.getMnemonic,
          address
        )
      case FLOAT_CEIL =>
        handleSingleArgument(
          diffGraphBuilder,
          nativeInstruction,
          pcodeOpAst,
          "<operator>.ceil",
          pcodeOpAst.getMnemonic,
          address
        )
      case FLOAT_FLOAT2FLOAT =>
        handleSingleArgument(
          diffGraphBuilder,
          nativeInstruction,
          pcodeOpAst,
          "<operator>.float2float",
          pcodeOpAst.getMnemonic,
          address
        )
      case FLOAT_FLOOR =>
        handleSingleArgument(
          diffGraphBuilder,
          nativeInstruction,
          pcodeOpAst,
          "<operator>.floor",
          pcodeOpAst.getMnemonic,
          address
        )
      case FLOAT_INT2FLOAT =>
        handleSingleArgument(
          diffGraphBuilder,
          nativeInstruction,
          pcodeOpAst,
          "<operator>.int2float",
          pcodeOpAst.getMnemonic,
          address
        )
      case FLOAT_LESS | INT_SLESS | INT_LESS =>
        handleTwoArguments(diffGraphBuilder, nativeInstruction, pcodeOpAst, "<operator>.less", "<")
      case FLOAT_LESSEQUAL | INT_SLESSEQUAL | INT_LESSEQUAL =>
        handleTwoArguments(diffGraphBuilder, nativeInstruction, pcodeOpAst, "<operator>.lessThanEqual", "<=")
      case FLOAT_NAN =>
        handleSingleArgument(
          diffGraphBuilder,
          nativeInstruction,
          pcodeOpAst,
          "<operator>.nan",
          pcodeOpAst.getMnemonic,
          address
        )
      case FLOAT_ROUND =>
        handleSingleArgument(
          diffGraphBuilder,
          nativeInstruction,
          pcodeOpAst,
          "<operator>.round",
          pcodeOpAst.getMnemonic,
          address
        )
      case FLOAT_SQRT =>
        handleSingleArgument(
          diffGraphBuilder,
          nativeInstruction,
          pcodeOpAst,
          "<operator>.sqrt",
          pcodeOpAst.getMnemonic,
          address
        )
      case FLOAT_TRUNC =>
        handleSingleArgument(
          diffGraphBuilder,
          nativeInstruction,
          pcodeOpAst,
          "<operator>.trunc",
          pcodeOpAst.getMnemonic,
          address
        )
      case INSERT =>
        handleSingleArgument(
          diffGraphBuilder,
          nativeInstruction,
          pcodeOpAst,
          "<operator>.insert",
          pcodeOpAst.getMnemonic,
          address
        )
      case INT_2COMP =>
        handleSingleArgument(
          diffGraphBuilder,
          nativeInstruction,
          pcodeOpAst,
          "<operator>.int2comp",
          pcodeOpAst.getMnemonic,
          address
        )
      case INT_ADD | FLOAT_ADD | PTRADD =>
        handleTwoArguments(diffGraphBuilder, nativeInstruction, pcodeOpAst, "<operator>.addition", "+")
      case INT_AND =>
        handleTwoArguments(diffGraphBuilder, nativeInstruction, pcodeOpAst, "<operator>.and", "TODO: AND")
      case INT_CARRY =>
        handleTwoArguments(diffGraphBuilder, nativeInstruction, pcodeOpAst, "<operator>.TODO", "TODO: INT_CARRY")
      case INT_DIV | FLOAT_DIV | INT_SDIV =>
        handleTwoArguments(diffGraphBuilder, nativeInstruction, pcodeOpAst, "<operator>.division", "/")
      case FLOAT_EQUAL | INT_EQUAL =>
        handleTwoArguments(diffGraphBuilder, nativeInstruction, pcodeOpAst, "<operator>.equal", "==")
      case INT_NOTEQUAL | FLOAT_NOTEQUAL =>
        handleTwoArguments(diffGraphBuilder, nativeInstruction, pcodeOpAst, "<operator>.notEqual", "!=")
      case INT_LEFT =>
        handleTwoArguments(diffGraphBuilder, nativeInstruction, pcodeOpAst, "<operator>.shiftLeft", "<<")
      case INT_MULT | FLOAT_MULT =>
        handleTwoArguments(diffGraphBuilder, nativeInstruction, pcodeOpAst, "<operator>.multiplication", "*")
      case FLOAT_NEG | INT_NEGATE =>
        handleSingleArgument(
          diffGraphBuilder,
          nativeInstruction,
          pcodeOpAst,
          "<operator>.negation",
          pcodeOpAst.getMnemonic,
          address
        )
      case INT_OR =>
        handleTwoArguments(diffGraphBuilder, nativeInstruction, pcodeOpAst, "<operator>.or", "||")
      case INT_REM | INT_SREM =>
        handleTwoArguments(diffGraphBuilder, nativeInstruction, pcodeOpAst, "<operator>.modolo", "%")
      case INT_RIGHT | INT_SRIGHT =>
        handleTwoArguments(diffGraphBuilder, nativeInstruction, pcodeOpAst, "<operator>.shiftRight", ">>")
      case INT_SBORROW =>
        handleSingleArgument(
          diffGraphBuilder,
          nativeInstruction,
          pcodeOpAst,
          "<operator>.sborrow",
          pcodeOpAst.getMnemonic,
          address
        )
      case INT_SCARRY =>
        handleSingleArgument(
          diffGraphBuilder,
          nativeInstruction,
          pcodeOpAst,
          "<operator>.scarry",
          pcodeOpAst.getMnemonic,
          address
        )
      case INT_SEXT | INT_ZEXT =>
        handleSingleArgument(
          diffGraphBuilder,
          nativeInstruction,
          pcodeOpAst,
          "<operator>.extend",
          pcodeOpAst.getMnemonic,
          address
        )
      case INT_SUB | FLOAT_SUB | PTRSUB =>
        handleTwoArguments(diffGraphBuilder, nativeInstruction, pcodeOpAst, "<operator>.subtraction", "-")
      case INT_XOR =>
        // TODO
        handleTwoArguments(diffGraphBuilder, nativeInstruction, pcodeOpAst, "<operator>.xor", "^")
      case COPY | LOAD | SUBPIECE =>
        handleAssignment(diffGraphBuilder, nativeInstruction, pcodeOpAst)
      case STORE =>
        handleStore(diffGraphBuilder, nativeInstruction, pcodeOpAst)
      case NEW =>
        handleSingleArgument(
          diffGraphBuilder,
          nativeInstruction,
          pcodeOpAst,
          "<operator>.new",
          pcodeOpAst.getMnemonic,
          address
        )
      case UNIMPLEMENTED | SEGMENTOP | MULTIEQUAL | INDIRECT | PIECE | PCODE_MAX =>
        createCall(
          "TODO: UNIMPLEMENTED | SEGMENTOP | MULTIEQUAL | INDIRECT | PIECE | PCODE_MAX | POPCOUNT ",
          "TODO: UNIMPLEMENTED | SEGMENTOP | MULTIEQUAL | INDIRECT | PIECE | PCODE_MAX | POPCOUNT ",
          nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue()
        )
      case POPCOUNT =>
        handleAssignment(diffGraphBuilder, nativeInstruction, pcodeOpAst)
      case _ =>
        createCall("NOT HANDLED", "NOT HANDLED", nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue())
    }
    callNode
  }
  def handlePcodeOpAst(
    diffGraphBuilder: DiffGraphBuilder,
    instruction: Instruction,
    pcodeOpAst: PcodeOpAST,
    highFunction: HighFunction
  ): CfgNodeNew = {
    mapCallNode(
      diffGraphBuilder,
      instruction,
      pcodeOpAst,
      instruction.getMinAddress.getOffsetAsBigInteger.intValue(),
      highFunction
    )
  }
  def handleBody(
    diffGraphBuilder: DiffGraphBuilder,
    function: Function,
    methodNode: NewMethod,
    blockNode: NewBlock
  ): Unit = {
    // get asm instructions
    val pcodeAsts = getHighFunction(function).getPcodeOps.asScala.toList.map(pcodeOpAst =>
      handlePcodeOpAst(diffGraphBuilder, null, pcodeOpAst, getHighFunction(function))
    )
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
        diffGraphBuilder.addEdge(blockNode, prevInstructionNode, EdgeTypes.AST)
        diffGraphBuilder.addEdge(methodNode, prevInstructionNode, EdgeTypes.CFG)
        diffGraphBuilder.addEdge(blockNode, instructionNode, EdgeTypes.AST)
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
