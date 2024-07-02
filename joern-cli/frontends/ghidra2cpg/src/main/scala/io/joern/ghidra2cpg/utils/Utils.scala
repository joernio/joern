package io.joern.ghidra2cpg.utils

import ghidra.program.model.listing.{Function, Instruction, Program}
import io.joern.ghidra2cpg.Types
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.proto.cpg.Cpg.DispatchTypes

import scala.jdk.CollectionConverters.*
import scala.language.implicitConversions

object Utils {
  def createCallNode(code: String, name: String, lineNumber: Integer, index: Int = -1): NewCall = {
    NewCall()
      .name(name)
      .code(code)
      .order(0)
      .argumentIndex(index)
      .methodFullName(name)
      .dispatchType(DispatchTypes.STATIC_DISPATCH.name())
      .lineNumber(lineNumber)
  }

  def createParameterNode(
    code: String,
    name: String,
    order: Int,
    typ: String,
    lineNumber: Int
  ): NewMethodParameterIn = {
    NewMethodParameterIn()
      .code(code)
      .name(name)
      .order(order)
      .typeFullName(Types.registerType(typ))
      .lineNumber(lineNumber)
  }

  def createIdentifier(code: String, name: String, index: Int, typ: String, lineNumber: Int): NewIdentifier = {
    NewIdentifier()
      .code(code)
      .name(name)
      .order(index)
      .argumentIndex(index)
      .typeFullName(Types.registerType(typ))
    // .lineNumber(lineNumber)
  }

  def createReturnNode(code: String, lineNumber: Integer): NewReturn = {
    NewReturn()
      .code(code)
      .order(0)
      .argumentIndex(0)
      .lineNumber(lineNumber)
  }
  def createLiteral(code: String, order: Int, argumentIndex: Int, typeFullName: String, lineNumber: Int): NewLiteral = {
    NewLiteral()
      .code(code)
      .order(order)
      .argumentIndex(argumentIndex)
      .typeFullName(typeFullName)
    // .lineNumber(lineNumber)
  }
  def createReturnNode(): NewMethodReturn = {
    NewMethodReturn().order(1)
  }
  def createMethodNode(decompiler: Decompiler, function: Function, fileName: String, isExternal: Boolean): NewMethod = {
    val code = decompiler.toDecompiledFunction(function).map(_.getC).getOrElse("")
    val lineNumberEnd = Option(function.getReturn)
      .flatMap(x => Option(x.getMinAddress))
      .flatMap(x => Option(x.getOffsetAsBigInteger))
      .flatMap(x => Option(x.intValue()))
      .getOrElse(-1)

    NewMethod()
      .code(code)
      .name(function.getName)
      .fullName(function.getName)
      .isExternal(isExternal)
      .signature(function.getSignature(true).toString)
      .lineNumber(function.getEntryPoint.getOffsetAsBigInteger.intValue())
      .columnNumber(-1)
      .lineNumberEnd(lineNumberEnd)
      .order(0)
      .filename(fileName)
      .astParentType(NodeTypes.NAMESPACE_BLOCK)
      .astParentFullName(s"$fileName:<global>")
  }
  def checkIfExternal(currentProgram: Program, functionName: String): Boolean = {
    currentProgram.getFunctionManager.getExternalFunctions
      .iterator()
      .asScala
      .map(_.getName)
      .contains(functionName)
  }
  def getInstructions(program: Program, function: Function): Seq[Instruction] =
    program.getListing.getInstructions(function.getBody, true).iterator().asScala.toList

}
