package io.joern.x2cpg.utils

import io.shiftleft.codepropertygraph.generated.nodes.Call.PropertyDefaults
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewAnnotationLiteral,
  NewCall,
  NewFieldIdentifier,
  NewIdentifier,
  NewMethodReturn,
  NewModifier
}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EvaluationStrategies}

object NodeBuilders {

  private def composeCallSignature(returnType: String, argumentTypes: Iterable[String]): String = {
    s"$returnType(${argumentTypes.mkString(",")})"
  }

  private def composeMethodFullName(typeDeclFullName: Option[String], name: String, signature: String) = {
    val typeDeclPrefix = typeDeclFullName.map(maybeName => s"$maybeName.").getOrElse("")
    s"$typeDeclPrefix$name:$signature"
  }

  def annotationLiteralNode(name: String): NewAnnotationLiteral =
    NewAnnotationLiteral()
      .name(name)
      .code(name)

  def callNode(
    methodName: String,
    typeDeclFullName: Option[String],
    returnTypeFullName: String,
    dispatchType: String,
    argumentTypes: Iterable[String] = Nil,
    code: String = PropertyDefaults.Code,
    lineNumber: Option[Integer] = None,
    columnNumber: Option[Integer] = None
  ): NewCall = {
    val signature      = composeCallSignature(returnTypeFullName, argumentTypes)
    val methodFullName = composeMethodFullName(typeDeclFullName, methodName, signature)
    NewCall()
      .name(methodName)
      .methodFullName(methodFullName)
      .signature(signature)
      .typeFullName(returnTypeFullName)
      .dispatchType(dispatchType)
      .code(code)
      .lineNumber(lineNumber)
      .columnNumber(columnNumber)
  }

  def fieldIdentifierNode(
    name: String,
    line: Option[Integer] = None,
    column: Option[Integer] = None
  ): NewFieldIdentifier = {
    NewFieldIdentifier()
      .canonicalName(name)
      .code(name)
      .lineNumber(line)
      .columnNumber(column)
  }

  def identifierNode(
    name: String,
    typeFullName: Option[String],
    line: Option[Integer] = None,
    column: Option[Integer] = None,
    dynamicTypeHintFullName: Seq[String] = Seq.empty
  ): NewIdentifier = {
    val identifier = NewIdentifier()
      .name(name)
      .code(name)
      .lineNumber(line)
      .columnNumber(column)
      .dynamicTypeHintFullName(dynamicTypeHintFullName)

    typeFullName.map(identifier.typeFullName(_))
    identifier
  }

  def modifierNode(modifierType: String): NewModifier = NewModifier().modifierType(modifierType)

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
      .typeFullName(typeFullName.getOrElse(PropertyDefaults.TypeFullName))
      .lineNumber(line)
      .columnNumber(column)
  }

  /** Create a method return node
    */
  def methodReturnNode(
    typeFullName: String,
    dynamicTypeHintFullName: Option[String] = None,
    line: Option[Integer],
    column: Option[Integer]
  ): NewMethodReturn =
    NewMethodReturn()
      .typeFullName(typeFullName)
      .dynamicTypeHintFullName(dynamicTypeHintFullName)
      .code("RET")
      .evaluationStrategy(EvaluationStrategies.BY_VALUE)
      .lineNumber(line)
      .columnNumber(column)
}
