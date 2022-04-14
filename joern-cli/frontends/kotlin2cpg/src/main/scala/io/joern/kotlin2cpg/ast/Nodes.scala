package io.joern.kotlin2cpg.ast

import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EvaluationStrategies}
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewBlock,
  NewCall,
  NewControlStructure,
  NewFieldIdentifier,
  NewIdentifier,
  NewLiteral,
  NewLocal,
  NewMember,
  NewMethodParameterIn,
  NewMethodReturn,
  NewReturn,
  NewTypeRef
}

object Nodes {

  def identifierNode(name: String, typeFullName: String, line: Int = -1, column: Int = -1): NewIdentifier = {
    NewIdentifier()
      .code(name)
      .name(name)
      .typeFullName(typeFullName)
      .lineNumber(line)
      .columnNumber(column)
  }

  def localNode(
    name: String,
    typeFullName: String,
    closureBindingId: Option[String] = None,
    line: Int = -1,
    column: Int = -1
  ): NewLocal = {
    NewLocal()
      .code(name)
      .name(name)
      .typeFullName(typeFullName)
      .closureBindingId(closureBindingId)
      .lineNumber(line)
      .columnNumber(column)
  }

  def literalNode(code: String, typeFullName: String, line: Int = -1, column: Int = -1): NewLiteral = {
    NewLiteral()
      .code(code)
      .typeFullName(typeFullName)
      .lineNumber(line)
      .columnNumber(column)
  }

  def controlStructureNode(code: String, _type: String, line: Int = -1, column: Int = -1): NewControlStructure = {
    NewControlStructure()
      .code(code)
      .controlStructureType(_type)
      .lineNumber(line)
      .columnNumber(column)
  }

  def fieldIdentifierNode(name: String, line: Int = -1, column: Int = -1): NewFieldIdentifier = {
    NewFieldIdentifier()
      .code(name)
      .canonicalName(name)
      .lineNumber(line)
      .columnNumber(column)
  }

  def methodParameterNode(
    name: String,
    typeFullName: String,
    line: Int = -1,
    column: Int = -1
  ): NewMethodParameterIn = {
    NewMethodParameterIn()
      .name(name)
      .code(name)
      .evaluationStrategy(EvaluationStrategies.BY_VALUE)
      .typeFullName(typeFullName)
      .lineNumber(line)
      .columnNumber(column)
  }

  def memberNode(name: String, typeFullName: String, line: Int = -1, column: Int = -1): NewMember = {
    NewMember()
      .name(name)
      .code(name)
      .typeFullName(typeFullName)
      .lineNumber(line)
      .columnNumber(column)
  }

  def typeRefNode(code: String, typeFullName: String, line: Int = -1, column: Int = -1): NewTypeRef = {
    NewTypeRef()
      .code(code)
      .typeFullName(typeFullName)
      .lineNumber(line)
      .columnNumber(column)
  }

  def methodReturnNode(
    typeFullName: String,
    dynamicTypeHintFullName: Option[String],
    line: Int = -1,
    column: Int = -1
  ): NewMethodReturn = {
    NewMethodReturn()
      .code("RET")
      .evaluationStrategy(EvaluationStrategies.BY_VALUE)
      .typeFullName(typeFullName)
      .dynamicTypeHintFullName(dynamicTypeHintFullName.toList)
      .lineNumber(line)
      .columnNumber(column)
  }

  def returnNode(code: String, line: Int = -1, column: Int = -1): NewReturn = {
    NewReturn()
      .code(code)
      .lineNumber(line)
      .columnNumber(column)
  }

  def blockNode(code: String, typeFullName: String, line: Int = -1, column: Int = -1): NewBlock = {
    NewBlock()
      .code(code)
      .typeFullName(typeFullName)
      .lineNumber(line)
      .columnNumber(column)
  }

  def operatorCallNode(name: String, code: String, line: Int = -1, column: Int = -1): NewCall = {
    NewCall()
      .name(name)
      .methodFullName(name)
      .code(code)
      .signature("")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .typeFullName("ANY")
      .lineNumber(line)
      .columnNumber(column)
  }
}
