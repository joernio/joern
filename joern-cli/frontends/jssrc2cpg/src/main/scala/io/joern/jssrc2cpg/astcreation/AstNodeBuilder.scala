package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.datastructures.scope.MethodScope
import io.joern.jssrc2cpg.passes.Defines
import AstCreatorHelper.OptionSafeAst
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.Operators

trait AstNodeBuilder {

  this: AstCreator =>

  protected def newUnknown(node: BabelNodeInfo, order: Int): NewUnknown =
    NewUnknown()
      .parserTypeName(node.node.toString)
      .code(node.code)
      .order(order)
      .argumentIndex(order)
      .argumentIndex(order)
      .lineNumber(node.lineNumber)
      .columnNumber(node.columnNumber)

  protected def newTypeDecl(
    name: String,
    fullname: String,
    filename: String,
    code: String,
    astParentType: String = "",
    astParentFullName: String = "",
    order: Int = -1,
    inherits: Seq[String] = Seq.empty,
    alias: Option[String] = None,
    line: Option[Integer] = None,
    column: Option[Integer] = None
  ): NewTypeDecl =
    NewTypeDecl()
      .name(name)
      .fullName(fullname)
      .code(code)
      .isExternal(false)
      .filename(filename)
      .astParentType(astParentType)
      .astParentFullName(astParentFullName)
      .inheritsFromTypeFullName(inherits)
      .aliasTypeFullName(alias)
      .lineNumber(line)
      .columnNumber(column)
      .order(order)

  protected def createParameterInNode(
    name: String,
    code: String,
    order: Int,
    line: Option[Integer] = None,
    column: Option[Integer] = None,
    tpe: Option[String] = None
  ): NewMethodParameterIn = {
    val param = NewMethodParameterIn()
      .name(name)
      .code(code)
      .evaluationStrategy(EvaluationStrategies.BY_VALUE)
      .lineNumber(line)
      .columnNumber(column)
      .order(order)
      .typeFullName(tpe.getOrElse(Defines.ANY.label))
    scope.addVariable(name, param, MethodScope)
    param
  }

  protected def codeOf(node: NewNode): String = node match {
    case code: HasCode => code.code
    case _             => ""
  }

  protected def createFieldAccessNode(
    baseId: NewNode,
    partId: NewNode,
    order: Int,
    line: Option[Integer] = None,
    column: Option[Integer] = None
  ): Ast = {
    val call = createCallNode(
      codeOf(baseId) + "." + codeOf(partId),
      Operators.fieldAccess,
      DispatchTypes.STATIC_DISPATCH,
      line,
      column
    ).order(order).argumentIndex(order)
    Ast(call).withChild(Ast(baseId)).withChild(Ast(partId)).withArgEdge(call, baseId).withArgEdge(call, partId)
  }

  protected def createCallNode(
    code: String,
    callName: String,
    dispatchType: String,
    line: Option[Integer] = None,
    column: Option[Integer] = None
  ): NewCall = NewCall()
    .code(code)
    .name(callName)
    .methodFullName(callName)
    .dispatchType(dispatchType)
    .lineNumber(line)
    .columnNumber(column)
    .typeFullName(Defines.ANY.label)

  protected def createFieldIdentifierNode(
    name: String,
    line: Option[Integer] = None,
    column: Option[Integer] = None
  ): NewFieldIdentifier = NewFieldIdentifier()
    .code(name)
    .canonicalName(name)
    .lineNumber(line)
    .columnNumber(column)

  protected def createLiteralNode(
    code: String,
    dynamicTypeOption: Option[String],
    order: Int,
    line: Option[Integer] = None,
    column: Option[Integer] = None
  ): NewLiteral =
    NewLiteral()
      .code(code)
      .order(order)
      .argumentIndex(order)
      .typeFullName(Defines.ANY.label)
      .lineNumber(line)
      .columnNumber(column)
      .dynamicTypeHintFullName(dynamicTypeOption.toList)

  protected def createAssignment(
    destId: NewNode,
    sourceId: Seq[Ast],
    code: String,
    order: Int,
    line: Option[Integer] = None,
    column: Option[Integer] = None
  ): Ast = {
    val call = createCallNode(code, Operators.assignment, DispatchTypes.STATIC_DISPATCH, line, column)
      .order(order)
      .argumentIndex(order)
    Ast(call).withChild(Ast(destId)).withChildren(sourceId).withArgEdge(call, destId).withArgEdges(call, sourceId)
  }

  protected def createIdentifierNode(name: String, node: BabelNodeInfo): NewIdentifier = {
    val dynamicInstanceTypeOption = name match {
      case "this" =>
        dynamicInstanceTypeStack.headOption
      case "console" =>
        Some(Defines.CONSOLE.label)
      case "Math" =>
        Some(Defines.MATH.label)
      case _ =>
        None
    }

    createIdentifierNode(name, dynamicInstanceTypeOption, node.lineNumber, node.columnNumber)
  }

  protected def createIdentifierNode(
    name: String,
    dynamicTypeOption: Option[String],
    line: Option[Integer] = None,
    column: Option[Integer] = None
  ): NewIdentifier = NewIdentifier()
    .name(name)
    .code(name)
    .lineNumber(line)
    .columnNumber(column)
    .typeFullName(Defines.ANY.label)
    .dynamicTypeHintFullName(dynamicTypeOption.toList)

  protected def createStaticCallNode(
    code: String,
    methodName: String,
    fullName: String,
    order: Int,
    line: Option[Integer] = None,
    column: Option[Integer] = None
  ): NewCall = NewCall()
    .code(code)
    .name(methodName)
    .order(order)
    .argumentIndex(order)
    .methodFullName(fullName)
    .dispatchType(DispatchTypes.STATIC_DISPATCH)
    .signature("")
    .lineNumber(line)
    .columnNumber(column)
    .typeFullName(Defines.ANY.label)

  protected def createLocalNode(
    name: String,
    typeFullName: String,
    order: Int,
    closureBindingId: Option[String] = None
  ): NewLocal =
    NewLocal().code(name).name(name).typeFullName(typeFullName).closureBindingId(closureBindingId).order(order)

  protected def createClosureBindingNode(closureBindingId: String, closureOriginalName: String): NewClosureBinding =
    NewClosureBinding()
      .closureBindingId(Some(closureBindingId))
      .evaluationStrategy(EvaluationStrategies.BY_REFERENCE)
      .closureOriginalName(Some(closureOriginalName))

  protected def createTypeNode(name: String, fullName: String): NewType =
    NewType().name(name).fullName(fullName).typeDeclFullName(fullName)

  protected def createBindingNode(): NewBinding = NewBinding().name("").signature("")

  protected def createFunctionTypeAndTypeDeclAst(
    methodNode: NewMethod,
    parentNode: NewNode,
    methodName: String,
    methodFullName: String,
    filename: String
  ): Ast = {
    val typeNode = createTypeNode(methodName, methodFullName)
    Ast.storeInDiffGraph(Ast(typeNode), diffGraph)

    val astParentType     = parentNode.label
    val astParentFullName = parentNode.properties("FULL_NAME").toString
    val functionTypeDeclNode =
      newTypeDecl(
        methodName,
        methodFullName,
        filename,
        methodName,
        astParentType = astParentType,
        astParentFullName = astParentFullName,
        order = 0
      ).inheritsFromTypeFullName(List(Defines.ANY.label))
    // Problem for https://github.com/ShiftLeftSecurity/codescience/issues/3626 here.
    // As the type (thus, the signature) of the function node is unknown (i.e., ANY*)
    // we can't generate the correct binding with signature.
    val bindingNode = createBindingNode()
    Ast(functionTypeDeclNode).withBindsEdge(functionTypeDeclNode, bindingNode).withRefEdge(bindingNode, methodNode)
  }

}
