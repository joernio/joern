package io.joern.kotlin2cpg.ast

import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EvaluationStrategies}
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewBinding,
  NewBlock,
  NewCall,
  NewClosureBinding,
  NewControlStructure,
  NewFieldIdentifier,
  NewIdentifier,
  NewJumpTarget,
  NewLiteral,
  NewLocal,
  NewMember,
  NewMethod,
  NewMethodParameterIn,
  NewMethodRef,
  NewMethodReturn,
  NewModifier,
  NewNamespaceBlock,
  NewReturn,
  NewTypeDecl,
  NewTypeRef,
  NewUnknown
}

object Nodes {

  def bindingNode(name: String, signature: String): NewBinding = {
    NewBinding()
      .name(name)
      .signature(signature)
  }

  def blockNode(code: String, typeFullName: String, line: Int = -1, column: Int = -1): NewBlock = {
    NewBlock()
      .code(code)
      .typeFullName(typeFullName)
      .lineNumber(line)
      .columnNumber(column)
  }

  def callNode(
    code: String,
    name: String,
    methodFullName: String,
    signature: String,
    typeFullName: String,
    dispatchType: String,
    line: Int = -1,
    column: Int = -1
  ): NewCall = {
    NewCall()
      .code(code)
      .name(name)
      .methodFullName(methodFullName)
      .signature(signature)
      .dispatchType(dispatchType)
      .typeFullName(typeFullName)
      .lineNumber(line)
      .columnNumber(column)
  }

  def closureBinding(closureBindingId: String, originalName: String): NewClosureBinding = {
    NewClosureBinding()
      .closureBindingId(closureBindingId)
      .closureOriginalName(originalName)
      .evaluationStrategy(EvaluationStrategies.BY_REFERENCE)
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

  def identifierNode(name: String, typeFullName: String, line: Int = -1, column: Int = -1): NewIdentifier = {
    NewIdentifier()
      .code(name)
      .name(name)
      .typeFullName(typeFullName)
      .lineNumber(line)
      .columnNumber(column)
  }

  def jumpTargetNode(
    code: String,
    name: String,
    parserTypeName: String,
    line: Int = -1,
    column: Int = -1
  ): NewJumpTarget = {
    NewJumpTarget()
      .code(code)
      .name(name)
      .parserTypeName(parserTypeName)
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

  def methodNode(
    name: String,
    fullName: String,
    signature: String,
    fileName: String,
    line: Int = -1,
    column: Int = -1,
    lineEnd: Int = -1,
    columnEnd: Int = -1
  ): NewMethod = {
    NewMethod()
      .code(name)
      .name(name)
      .fullName(fullName)
      .signature(signature)
      .filename(fileName)
      .isExternal(false)
      .lineNumber(line)
      .columnNumber(column)
      .lineNumberEnd(lineEnd)
      .columnNumberEnd(columnEnd)
  }

  def memberNode(name: String, typeFullName: String, line: Int = -1, column: Int = -1): NewMember = {
    NewMember()
      .name(name)
      .code(name)
      .typeFullName(typeFullName)
      .lineNumber(line)
      .columnNumber(column)
  }

  def methodRefNode(
    name: String,
    fullName: String,
    typeFullName: String,
    line: Int = -1,
    column: Int = -1
  ): NewMethodRef = {
    NewMethodRef()
      .code(name)
      .methodFullName(fullName)
      .typeFullName(typeFullName)
      .lineNumber(line)
      .columnNumber(column)
  }

  def modifierNode(_type: String): NewModifier = {
    NewModifier()
      .modifierType(_type)
  }

  def namespaceBlockNode(name: String, fullName: String, fileName: String): NewNamespaceBlock = {
    NewNamespaceBlock()
      .name(name)
      .fullName(fullName)
      .filename(fileName)
  }

  def operatorCallNode(
    name: String,
    code: String,
    typeFullName: Option[String] = None,
    line: Int = -1,
    column: Int = -1
  ): NewCall = {
    NewCall()
      .name(name)
      .methodFullName(name)
      .code(code)
      .signature("")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .typeFullName(typeFullName.getOrElse("ANY"))
      .lineNumber(line)
      .columnNumber(column)
  }

  def returnNode(code: String, line: Int = -1, column: Int = -1): NewReturn = {
    NewReturn()
      .code(code)
      .lineNumber(line)
      .columnNumber(column)
  }

  def typeDeclNode(
    name: String,
    fullName: String,
    fileName: String,
    inheritsFromFullNames: collection.Seq[String],
    aliasTypeFullName: Option[String] = None,
    line: Int = -1,
    column: Int = -1
  ): NewTypeDecl = {
    NewTypeDecl()
      .code(name)
      .name(name)
      .fullName(fullName)
      .filename(fileName)
      .inheritsFromTypeFullName(inheritsFromFullNames)
      .aliasTypeFullName(aliasTypeFullName)
      .isExternal(false)
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

  def unknownNode(code: String, parserTypeName: String, line: Int = -1, column: Int = -1): NewUnknown = {
    NewUnknown()
      .code(code)
      .parserTypeName(parserTypeName)
      .typeFullName("ANY")
      .lineNumber(line)
      .columnNumber(column)
  }
}
