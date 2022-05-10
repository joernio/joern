package io.joern.ghidra2cpg.utils

import ghidra.program.model.listing.{CodeUnitFormat, CodeUnitFormatOptions, Function, Instruction}
import ghidra.program.model.pcode.PcodeOp._
import ghidra.program.model.pcode.{PcodeOp, Varnode}
import io.joern.ghidra2cpg.Types
import io.joern.ghidra2cpg.utils.Utils.{createCallNode, createIdentifier, createLiteral}
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.CfgNodeNew
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate.DiffGraphBuilder

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.language.implicitConversions

class PCodeMapper(
                   diffGraphBuilder: DiffGraphBuilder,
                   nativeInstruction: Instruction,
                   functions: List[Function],
                   decompiler: Decompiler
                 ) {
  private val logger = LoggerFactory.getLogger(getClass)
  var nodeStack: mutable.HashMap[String, CfgNodeNew] = new mutable.HashMap[String, CfgNodeNew]()
  private val pcodeOps: Array[PcodeOp] = nativeInstruction.getPcode()
  val codeUnitFormat = new CodeUnitFormat(
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

  def getInstruction: Instruction = nativeInstruction

  def getPcodeOps: Array[PcodeOp] = pcodeOps

  def getOpcode: Int = pcodeOps.lastOption.get.getOpcode

  def getCallNode: CfgNodeNew = {
    if (pcodeOps.isEmpty) {
      // It looks like that for some instructions,
      // like "bti c" getPcode() returns nothing
      logger.info(s"NO pcodes for $nativeInstruction")
      createCallNode(
        nativeInstruction.toString,
        nativeInstruction.toString,
        nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue()
      )
    } else if (pcodeOps.length == 1) {
      // we don't need to fill the hashmap for one PcodeOp
      // map to node and return
      mapCallNode(pcodeOps.head)
    } else {
      // Iterating over the pcodeOps and replace the "unique" outputs
      // with the the according node
      // e.g. the instruction "lw t9,-0x7ff0(gp)" results in the following pcodes:
      //
      // 0 : "(unique, 0x100, 4) INT_ADD (register, 0x70, 4) , (const, 0xffff8010, 4)"
      // 1 : "(unique, 0x180, 4) COPY (const, 0x0, 4)"
      // 2 : "(unique, 0x180, 4) COPY (unique, 0x100, 4)"
      // 3 : "(register, 0x64, 4) LOAD (const, 0x1a1, 8) , (unique, 0x180, 4)"
      //
      // We want to return:
      // NewCall(code=lw t9,-0x7ff0(gp),name="<operator>.assignment")
      //   Parameter 1: NewIdentifier(code=t9)
      //   Parameter 2: NewCall(code=-0x7ff0(gp), name="<operator>.addition")
      //                  Parameter 1: NewIdentifier(gp)
      //                  Parameter 2: NewLiteral(-0x7ff0)
      try {
        val resolvedVars = pcodeOps.last.getInputs.zipWithIndex.map { case (variable, index) => resolveVarNode(variable, index) }
        val last = pcodeOps.last
        val ret = last.getOpcode match {
          // TODO add more pcode ops like CALL.*
          case BRANCH | BRANCHIND | CBRANCH =>
            createCallNode(last.toString, "<operator>.goto", last.getSeqnum.getTarget.getOffsetAsBigInteger.intValue())
          case RETURN =>
            createCall("TODO RET", "TODO RET")
          case CALL | CALLOTHER | CALLIND =>
            val calledFunction = codeUnitFormat
              .getOperandRepresentationString(nativeInstruction, 0)
              .split(">")
              .last
              .replace("[", "")
              .replace("]", "")
            val callee = functions.filter(_.getName == calledFunction)
            val _callNode = createCallNode(calledFunction, calledFunction, nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue)
            if (callee.nonEmpty) {
              // Array of tuples containing (checked parameter name, parameter index, parameter data type)
              var checkedParameters = Array.empty[(String, Int, String)]

              if (callee.head.isThunk) {
                // thunk functions contain parameters already
                val parameters = callee.head.getParameters
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
                  nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue
                )
                connectCallToArgument(_callNode, node)
              }
            }
            _callNode
          case _ =>
            createCallNode("TODO", "TODO", -1)
        }
        resolvedVars.foreach(x => connectCallToArgument(ret, x))
        return ret
      } catch {
        case _: Exception => println(nativeInstruction)
      }
    }
    createCallNode("UNKNOWN", "UNKNOWN", -1)
  }

  def createCall(name: String, code: String): CfgNodeNew = {
    createCallNode(code, name, nativeInstruction.getMinAddress.getOffsetAsBigInteger.intValue)
  }

  def handleSingleArgument(pcodeOp: PcodeOp, cpgOperationName: String, operation: String): CfgNodeNew = {
    val firstOp = resolveVarNode(pcodeOp.getInput(0), 1)
    val callNode = createCall(cpgOperationName, s"$operation(${firstOp.code})")
    connectCallToArgument(callNode, firstOp)
    callNode
  }

  def handleTwoArguments(pcodeOp: PcodeOp, cpgOperationName: String, operation: String): CfgNodeNew = {
    val firstOp = resolveVarNode(pcodeOp.getInput(0), 1)
    val secondOp = resolveVarNode(pcodeOp.getInput(1), 2)
    val callNode = createCall(cpgOperationName, s"${firstOp.code} $operation ${secondOp.code}")
    connectCallToArgument(callNode, firstOp)
    connectCallToArgument(callNode, secondOp)
    callNode
  }

  def handleAssignment(pcodeOp: PcodeOp): CfgNodeNew = {
    val firstOp = createCallNode(pcodeOp.getOutput.toString, pcodeOp.getOutput.toString, pcodeOp.getSeqnum.getTarget.getOffsetAsBigInteger.intValue())//, 0)
    val secondOp = resolveVarNode(pcodeOp.getInputs.headOption.get, 1)
    val callNode = createCall("<operation>.assignment", s"${firstOp.code} = ${secondOp.code}")
    connectCallToArgument(callNode, firstOp)
    connectCallToArgument(callNode, secondOp)
    callNode
  }

  private def mapCallNode(pcodeOp: PcodeOp): CfgNodeNew = {
    //var callNode: CfgNodeNew = createCallNode("UNKNOWN", "UNKNOWN", -1)
    val callNode = pcodeOp.getOpcode match {
      case BOOL_AND =>
        handleTwoArguments(pcodeOp, "<operator>.and", "&&")
      case BOOL_NEGATE =>
        handleSingleArgument(pcodeOp, "<operator>.negate", pcodeOp.getMnemonic)
      case BOOL_OR =>
        handleTwoArguments(pcodeOp, "<operator>.or", "||")
      case BOOL_XOR =>
        handleTwoArguments(pcodeOp, "<operator>.xor", "^^")

      case CAST =>
        handleSingleArgument(pcodeOp, "<operator>.cast", pcodeOp.getMnemonic)
      case CPOOLREF =>
        handleSingleArgument(pcodeOp, "<operator>.cpoolref", pcodeOp.getMnemonic)
      case EXTRACT =>
        handleSingleArgument(pcodeOp, "<operator>.extract", pcodeOp.getMnemonic)
      case FLOAT_ABS =>
        handleSingleArgument(pcodeOp, "<operator>.abs", pcodeOp.getMnemonic)
      case FLOAT_CEIL =>
        handleSingleArgument(pcodeOp, "<operator>.ceil", pcodeOp.getMnemonic)
      case FLOAT_FLOAT2FLOAT =>
        handleSingleArgument(pcodeOp, "<operator>.float2float", pcodeOp.getMnemonic)
      case FLOAT_FLOOR =>
        handleSingleArgument(pcodeOp, "<operator>.floor", pcodeOp.getMnemonic)
      case FLOAT_INT2FLOAT =>
        handleSingleArgument(pcodeOp, "<operator>.int2float", pcodeOp.getMnemonic)
      case FLOAT_LESS | INT_SLESS | INT_LESS =>
        handleTwoArguments(pcodeOp, "<operator>.less", "<")
      case FLOAT_LESSEQUAL | INT_SLESSEQUAL | INT_LESSEQUAL =>
        handleTwoArguments(pcodeOp, "<operator>.lessThanEqual", "<=")
      case FLOAT_NAN =>
        handleSingleArgument(pcodeOp, "<operator>.nan", pcodeOp.getMnemonic)
      case FLOAT_ROUND =>
        handleSingleArgument(pcodeOp, "<operator>.round", pcodeOp.getMnemonic)
      case FLOAT_SQRT =>
        handleSingleArgument(pcodeOp, "<operator>.sqrt", pcodeOp.getMnemonic)
      case FLOAT_TRUNC =>
        handleSingleArgument(pcodeOp, "<operator>.trunc", pcodeOp.getMnemonic)
      case INSERT =>
        handleSingleArgument(pcodeOp, "<operator>.insert", pcodeOp.getMnemonic)
      case INT_2COMP =>
        handleSingleArgument(pcodeOp, "<operator>.int2comp", pcodeOp.getMnemonic)
      case INT_ADD | FLOAT_ADD | PTRADD =>
        handleTwoArguments(pcodeOp, "<operator>.addition", "+")
      case INT_AND =>
        handleTwoArguments(pcodeOp, "<operator>.and", "TODO: AND")
      case INT_CARRY =>
        handleTwoArguments(pcodeOp, "<operator>.TODO", "TODO: INT_CARRY")
      case INT_DIV | FLOAT_DIV | INT_SDIV =>
        handleTwoArguments(pcodeOp, "<operator>.division", "/")
      case FLOAT_EQUAL | INT_EQUAL =>
        handleTwoArguments(pcodeOp, "<operator>.equal", "==")
      case INT_NOTEQUAL | FLOAT_NOTEQUAL =>
        handleTwoArguments(pcodeOp, "<operator>.notEqual", "!=")
      case INT_LEFT =>
        handleTwoArguments(pcodeOp, "<operator>.shiftLeft", "<<")
      case INT_MULT | FLOAT_MULT =>
        handleTwoArguments(pcodeOp, "<operator>.multiplication", "*")
      case FLOAT_NEG | INT_NEGATE =>
        handleSingleArgument(pcodeOp, "<operator>.negation", pcodeOp.getMnemonic)
      case INT_OR =>
        handleTwoArguments(pcodeOp, "<operator>.or", "||")
      case INT_REM | INT_SREM =>
        handleTwoArguments(pcodeOp, "<operator>.modolo", "%")
      case INT_RIGHT | INT_SRIGHT =>
        handleTwoArguments(pcodeOp, "<operator>.shiftRight", ">>")
      case INT_SBORROW =>
        handleSingleArgument(pcodeOp, "<operator>.sborrow", pcodeOp.getMnemonic)
      case INT_SCARRY =>
        handleSingleArgument(pcodeOp, "<operator>.scarry", pcodeOp.getMnemonic)
      case INT_SEXT | INT_ZEXT =>
        handleSingleArgument(pcodeOp, "<operator>.extend", pcodeOp.getMnemonic)
      case INT_SUB | FLOAT_SUB | PTRSUB =>
        handleTwoArguments(pcodeOp, "<operator>.subtraction", "-")
      case INT_XOR =>
        // TODO
        handleTwoArguments(pcodeOp, "<operator>.xor", "^")
      case COPY | LOAD => //| STORE | SUBPIECE =>
        handleAssignment(pcodeOp)
      case NEW =>
        handleSingleArgument(pcodeOp, "<operator>.new", pcodeOp.getMnemonic)
      case UNIMPLEMENTED | SEGMENTOP | MULTIEQUAL | INDIRECT | PIECE | PCODE_MAX | POPCOUNT =>
        createCall("NOT HANDLED", "NOT HANDLED")
      case _ =>
        createCall("NOT HANDLED", "NOT HANDLED")
    }
    callNode
  }

  def resolveVarNode(input: Varnode, index: Int): CfgNodeNew = {
    if (input.isRegister) {
      // we only care about the name
      createIdentifier(
        nativeInstruction.getProgram.getRegister(input).getName,
        nativeInstruction.getProgram.getRegister(input).getName,
        index + 1,
        Types.registerType(nativeInstruction.getProgram.getRegister(input).getName),
        input.getAddress.getOffsetAsBigInteger.intValue
      )
    } else if (input.isUnique) {
      val uniques = pcodeOps.toList
        // If the argument is a unique,
        // we try to resolve it
        .filter(x => x.getOutput == input)
        // Sometimes the first parameter is equal to the return value
        // filtering those out for now
        .filterNot(x=>x.getInput(0) == input)
      mapCallNode(uniques.last)
    } else {
      // input.isConstant || input.isAddress || input.isUnique
      createLiteral(
        "0x" + input.getWordOffset.toHexString,
        index + 1,
        index + 1,
        Types.registerType(input.getWordOffset.toHexString),
        -1
      )
    }
  }

  def connectCallToArgument(call: CfgNodeNew, argument: CfgNodeNew): Unit = {
    diffGraphBuilder.addNode(argument)
    diffGraphBuilder.addEdge(call, argument, EdgeTypes.ARGUMENT)
    diffGraphBuilder.addEdge(call, argument, EdgeTypes.AST)
  }
}