package io.joern.ghidra2cpg.utils

import ghidra.program.model.listing.{Function, Program}
import io.joern.ghidra2cpg.Types
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes, nodes}
import io.shiftleft.passes.DiffGraph
import io.shiftleft.proto.cpg.Cpg.{DiffGraph, DispatchTypes}

import scala.jdk.CollectionConverters._
import scala.language.implicitConversions
object Nodes {
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

  def createParameterNode(code: String,
                          name: String,
                          order: Int,
                          typ: String,
                          lineNumber: Int): NewMethodParameterIn = {
    nodes
      .NewMethodParameterIn()
      .code(code)
      .name(code)
      .order(order)
      .typeFullName(Types.registerType(typ))
      .lineNumber(lineNumber)
  }

  def createIdentifier(code: String, name: String, index: Int, typ: String, lineNumber: Int): NewIdentifier = {
    nodes
      .NewIdentifier()
      .code(code)
      .name(name) //parameter.getName)
      .order(index)
      .argumentIndex(index)
      .typeFullName(Types.registerType(typ))
      .lineNumber(lineNumber)
  }

  def createReturnNode(code: String, lineNumber: Integer): NewReturn = {
    nodes
      .NewReturn()
      .code(code)
      .order(0)
      .argumentIndex(0)
      .lineNumber(lineNumber)
  }
  def createLiteral(code: String, order: Int, argumentIndex: Int, typeFullName: String, lineNumber: Int): NewLiteral = {
    nodes
      .NewLiteral()
      .code(code)
      .order(order)
      .argumentIndex(argumentIndex)
      .typeFullName(typeFullName)
      .lineNumber(lineNumber)
  }
  def createReturnNode(): NewMethodReturn = {
    nodes.NewMethodReturn().order(1)

  }
  def createMethodNode(function: Function, fileName: String, isExternal: Boolean): NewMethod = {
    nodes
      .NewMethod()
      .code(function.getName)
      .name(function.getName)
      .fullName(function.getSignature(true).toString)
      .isExternal(isExternal)
      .signature(function.getSignature(true).toString)
      .lineNumber(function.getEntryPoint.getOffsetAsBigInteger.intValue())
      .columnNumber(-1)
      .lineNumberEnd(function.getReturn.getMinAddress.getOffsetAsBigInteger.intValue())
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
}
