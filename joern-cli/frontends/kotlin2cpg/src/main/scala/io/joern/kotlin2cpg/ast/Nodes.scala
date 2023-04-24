package io.joern.kotlin2cpg.ast

import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EvaluationStrategies}
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewBinding,
  NewCall,
  NewClosureBinding,
  NewIdentifier,
  NewJumpTarget,
  NewLiteral,
  NewLocal,
  NewMember,
  NewMethod,
  NewMethodParameterIn,
  NewModifier,
  NewNamespaceBlock,
  NewTypeDecl,
  NewTypeRef,
  NewUnknown
}

object Nodes {

  def bindingNode(name: String, signature: String, methodFullName: String): NewBinding = {
    NewBinding()
      .name(name)
      .methodFullName(methodFullName)
      .signature(signature)
  }

  def callNode(
    code: String,
    name: String,
    methodFullName: String,
    signature: String,
    typeFullName: String,
    dispatchType: String,
    line: Option[Integer] = None,
    column: Option[Integer] = None
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

  def identifierNode(
    name: String,
    typeFullName: String,
    line: Option[Integer] = None,
    column: Option[Integer] = None
  ): NewIdentifier = {
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
    line: Option[Integer] = None,
    column: Option[Integer] = None
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
    line: Option[Integer] = None,
    column: Option[Integer] = None
  ): NewLocal = {
    NewLocal()
      .code(name)
      .name(name)
      .typeFullName(typeFullName)
      .closureBindingId(closureBindingId)
      .lineNumber(line)
      .columnNumber(column)
  }

  def literalNode(
    code: String,
    typeFullName: String,
    line: Option[Integer] = None,
    column: Option[Integer] = None
  ): NewLiteral = {
    NewLiteral()
      .code(code)
      .typeFullName(typeFullName)
      .lineNumber(line)
      .columnNumber(column)
  }

  def methodParameterNode(
    name: String,
    typeFullName: String,
    line: Option[Integer] = None,
    column: Option[Integer] = None
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
    line: Option[Integer] = None,
    column: Option[Integer] = None,
    lineEnd: Option[Integer] = None,
    columnEnd: Option[Integer] = None
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

  def memberNode(
    name: String,
    typeFullName: String,
    line: Option[Integer] = None,
    column: Option[Integer] = None
  ): NewMember = {
    NewMember()
      .name(name)
      .code(name)
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
    line: Option[Integer] = None,
    column: Option[Integer] = None
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

  def typeDeclNode(
    name: String,
    fullName: String,
    fileName: String,
    inheritsFromFullNames: collection.Seq[String],
    aliasTypeFullName: Option[String] = None,
    line: Option[Integer] = None,
    column: Option[Integer] = None
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

  def unknownNode(
    code: String,
    parserTypeName: String,
    line: Option[Integer] = None,
    column: Option[Integer] = None
  ): NewUnknown = {
    NewUnknown()
      .code(code)
      .parserTypeName(parserTypeName)
      .typeFullName("ANY")
      .lineNumber(line)
      .columnNumber(column)
  }
}
