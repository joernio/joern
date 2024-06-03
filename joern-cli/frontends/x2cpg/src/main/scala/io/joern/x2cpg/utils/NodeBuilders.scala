package io.joern.x2cpg.utils

import io.shiftleft.codepropertygraph.generated.nodes.Call.PropertyDefaults
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewAnnotationLiteral,
  NewBinding,
  NewCall,
  NewClosureBinding,
  NewDependency,
  NewFieldIdentifier,
  NewIdentifier,
  NewLocal,
  NewMethodParameterIn,
  NewMethodReturn,
  NewModifier
}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EvaluationStrategies}
import io.shiftleft.codepropertygraph.generated.nodes.NewNamespaceBlock

/** NodeBuilders helps with node creation and is intended to be used when functions from `x2cpg.AstCreatorBase` are not
  * appropriate; for example, in cases in which the node's line and column are _not_ set from the base ASTNode type of a
  * specific frontend.
  */
object NodeBuilders {

  private def composeCallSignature(returnType: String, argumentTypes: Iterable[String]): String = {
    s"$returnType(${argumentTypes.mkString(",")})"
  }

  private def composeMethodFullName(typeDeclFullName: Option[String], name: String, signature: String) = {
    val typeDeclPrefix = typeDeclFullName.map(maybeName => s"$maybeName.").getOrElse("")
    s"$typeDeclPrefix$name:$signature"
  }

  def newAnnotationLiteralNode(name: String): NewAnnotationLiteral =
    NewAnnotationLiteral()
      .name(name)
      .code(name)

  def newBindingNode(name: String, signature: String, methodFullName: String): NewBinding = {
    NewBinding()
      .name(name)
      .methodFullName(methodFullName)
      .signature(signature)
  }

  def newLocalNode(name: String, typeFullName: String, closureBindingId: Option[String] = None): NewLocal = {
    NewLocal()
      .code(name)
      .name(name)
      .typeFullName(typeFullName)
      .closureBindingId(closureBindingId)
  }

  def newClosureBindingNode(
    closureBindingId: String,
    originalName: String,
    evaluationStrategy: String
  ): NewClosureBinding = {
    NewClosureBinding()
      .closureBindingId(closureBindingId)
      .closureOriginalName(originalName)
      .evaluationStrategy(evaluationStrategy)
  }

  def newCallNode(
    methodName: String,
    typeDeclFullName: Option[String],
    returnTypeFullName: String,
    dispatchType: String,
    argumentTypes: Iterable[String] = Nil,
    code: String = PropertyDefaults.Code,
    lineNumber: Option[Int] = None,
    columnNumber: Option[Int] = None
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

  def newDependencyNode(name: String, groupId: String, version: String): NewDependency =
    NewDependency()
      .name(name)
      .dependencyGroupId(groupId)
      .version(version)

  def newFieldIdentifierNode(name: String, line: Option[Int] = None, column: Option[Int] = None): NewFieldIdentifier = {
    NewFieldIdentifier()
      .canonicalName(name)
      .code(name)
      .lineNumber(line)
      .columnNumber(column)
  }

  def newModifierNode(modifierType: String): NewModifier = NewModifier().modifierType(modifierType)

  def newIdentifierNode(name: String, typeFullName: String, dynamicTypeHints: Seq[String] = Seq()): NewIdentifier = {
    newIdentifierNode(name, typeFullName, dynamicTypeHints, None)
  }

  def newIdentifierNode(
    name: String,
    typeFullName: String,
    dynamicTypeHints: Seq[String],
    line: Option[Int]
  ): NewIdentifier = {
    NewIdentifier()
      .code(name)
      .name(name)
      .typeFullName(typeFullName)
      .dynamicTypeHintFullName(dynamicTypeHints)
      .lineNumber(line)
  }

  def newOperatorCallNode(
    name: String,
    code: String,
    typeFullName: Option[String] = None,
    line: Option[Int] = None,
    column: Option[Int] = None
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

  def newThisParameterNode(
    name: String = "this",
    code: String = "this",
    typeFullName: String,
    dynamicTypeHintFullName: Seq[String] = Seq.empty,
    line: Option[Int] = None,
    column: Option[Int] = None,
    evaluationStrategy: String = EvaluationStrategies.BY_SHARING
  ): NewMethodParameterIn = {
    NewMethodParameterIn()
      .name(name)
      .code(code)
      .lineNumber(line)
      .columnNumber(column)
      .dynamicTypeHintFullName(dynamicTypeHintFullName)
      .evaluationStrategy(evaluationStrategy)
      .typeFullName(typeFullName)
      .index(0)
      .order(0)
  }

  /** Create a method return node
    */
  def newMethodReturnNode(
    typeFullName: String,
    dynamicTypeHintFullName: Option[String] = None,
    line: Option[Int],
    column: Option[Int]
  ): NewMethodReturn =
    NewMethodReturn()
      .typeFullName(typeFullName)
      .dynamicTypeHintFullName(dynamicTypeHintFullName)
      .code("RET")
      .evaluationStrategy(EvaluationStrategies.BY_VALUE)
      .lineNumber(line)
      .columnNumber(column)

  def newNamespaceBlockNode(name: String, fullName: String, fileName: String): NewNamespaceBlock = {
    NewNamespaceBlock()
      .name(name)
      .fullName(fullName)
      .filename(fileName)
  }
}
