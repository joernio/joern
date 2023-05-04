package io.joern.kotlin2cpg.ast

import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EvaluationStrategies}
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewCall,
  NewJumpTarget,
  NewMethodParameterIn,
  NewModifier,
  NewNamespaceBlock,
  NewTypeDecl
}

object Nodes {

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
}
