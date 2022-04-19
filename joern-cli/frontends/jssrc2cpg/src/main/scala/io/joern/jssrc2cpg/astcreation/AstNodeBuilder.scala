package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.datastructures.scope.MethodScope
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.jssrc2cpg.passes.Defines
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.Operators

trait AstNodeBuilder {

  this: AstCreator =>

  protected def newUnknown(node: BabelNodeInfo): NewUnknown =
    NewUnknown()
      .parserTypeName(node.node.toString)
      .code(node.code)
      .lineNumber(node.lineNumber)
      .columnNumber(node.columnNumber)

  protected def createTypeRefNode(code: String, typeFullName: String, classNode: BabelNodeInfo): NewTypeRef =
    NewTypeRef()
      .code(code)
      .typeFullName(typeFullName)
      .lineNumber(classNode.lineNumber)
      .columnNumber(classNode.columnNumber)

  protected def createTypeDeclNode(
    name: String,
    fullname: String,
    filename: String,
    code: String,
    astParentType: String = "",
    astParentFullName: String = "",
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

  protected def createReturnNode(ret: BabelNodeInfo): NewReturn =
    NewReturn()
      .code(ret.code)
      .lineNumber(ret.lineNumber)
      .columnNumber(ret.columnNumber)

  protected def createMethodReturnNode(func: BabelNodeInfo): NewMethodReturn = {
    val line   = func.lineNumber
    val column = func.columnNumber
    val code   = "RET"
    NewMethodReturn()
      .code(code)
      .evaluationStrategy(EvaluationStrategies.BY_VALUE)
      .typeFullName(Defines.ANY.label)
      .lineNumber(line)
      .columnNumber(column)
  }

  protected def createParameterInNode(
    name: String,
    code: String,
    index: Int,
    isVariadic: Boolean,
    line: Option[Integer] = None,
    column: Option[Integer] = None,
    tpe: Option[String] = None
  ): NewMethodParameterIn = {
    val param = NewMethodParameterIn()
      .name(name)
      .code(code)
      .index(index)
      .isVariadic(isVariadic)
      .evaluationStrategy(EvaluationStrategies.BY_VALUE)
      .lineNumber(line)
      .columnNumber(column)
      .typeFullName(tpe.getOrElse(Defines.ANY.label))
    scope.addVariable(name, param, MethodScope)
    param
  }

  protected def createMethodRefNode(code: String, methodFullName: String, func: BabelNodeInfo): NewMethodRef = {
    val line   = func.lineNumber
    val column = func.columnNumber
    NewMethodRef()
      .code(shortenCode(code))
      .methodFullName(methodFullName)
      .typeFullName(methodFullName)
      .lineNumber(line)
      .columnNumber(column)
  }

  protected def createMemberNode(name: String, code: String, dynamicTypeOption: Option[String]): NewMember =
    NewMember()
      .code(code)
      .name(name)
      .typeFullName(Defines.ANY.label)
      .dynamicTypeHintFullName(dynamicTypeOption.toList)

  protected def createMethodNode(methodName: String, methodFullName: String, func: BabelNodeInfo): NewMethod = {
    val line      = func.lineNumber
    val column    = func.columnNumber
    val lineEnd   = func.lineNumberEnd
    val columnEnd = func.columnNumberEnd
    val code      = func.code
    NewMethod()
      .name(methodName)
      .filename(parserResult.fullPath)
      .code(code)
      .fullName(methodFullName)
      .isExternal(false)
      .lineNumber(line)
      .columnNumber(column)
      .lineNumberEnd(lineEnd)
      .columnNumberEnd(columnEnd)
  }

  protected def codeOf(node: NewNode): String = node match {
    case code: HasCode => code.code
    case _             => ""
  }

  protected def createFieldAccess(
    baseId: NewNode,
    partId: NewNode,
    line: Option[Integer] = None,
    column: Option[Integer] = None
  ): Ast = {
    val callNode = createCallNode(
      codeOf(baseId) + "." + codeOf(partId),
      Operators.fieldAccess,
      DispatchTypes.STATIC_DISPATCH,
      line,
      column
    )
    callAst(callNode, List(Ast(baseId), Ast(partId)))
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
    line: Option[Integer] = None,
    column: Option[Integer] = None
  ): NewLiteral =
    NewLiteral()
      .code(code)
      .typeFullName(Defines.ANY.label)
      .lineNumber(line)
      .columnNumber(column)
      .dynamicTypeHintFullName(dynamicTypeOption.toList)

  protected def createAssignment(
    destId: NewNode,
    sourceId: NewNode,
    code: String,
    line: Option[Integer] = None,
    column: Option[Integer] = None
  ): Ast = {
    val callNode = createCallNode(code, Operators.assignment, DispatchTypes.STATIC_DISPATCH, line, column)
    callAst(callNode, List(Ast(destId), Ast(sourceId)))
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
    line: Option[Integer] = None,
    column: Option[Integer] = None
  ): NewCall = NewCall()
    .code(code)
    .name(methodName)
    .methodFullName(fullName)
    .dispatchType(DispatchTypes.STATIC_DISPATCH)
    .signature("")
    .lineNumber(line)
    .columnNumber(column)
    .typeFullName(Defines.ANY.label)

  protected def createLocalNode(name: String, typeFullName: String, closureBindingId: Option[String] = None): NewLocal =
    NewLocal().code(name).name(name).typeFullName(typeFullName).closureBindingId(closureBindingId).order(0)

  protected def createClosureBindingNode(closureBindingId: String, closureOriginalName: String): NewClosureBinding =
    NewClosureBinding()
      .closureBindingId(Some(closureBindingId))
      .evaluationStrategy(EvaluationStrategies.BY_REFERENCE)
      .closureOriginalName(Some(closureOriginalName))

  protected def createTypeNode(name: String, fullName: String): NewType =
    NewType().name(name).fullName(fullName).typeDeclFullName(fullName)

  protected def createBindingNode(): NewBinding = NewBinding().name("").signature("")

  protected def createBlockNode(code: String, line: Option[Integer] = None, column: Option[Integer] = None): NewBlock =
    NewBlock()
      .typeFullName(Defines.ANY.label)
      .code(code)
      .lineNumber(line)
      .columnNumber(column)

  protected def createFunctionTypeAndTypeDecl(
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
      createTypeDeclNode(
        methodName,
        methodFullName,
        filename,
        methodName,
        astParentType = astParentType,
        astParentFullName = astParentFullName
      ).inheritsFromTypeFullName(List(Defines.ANY.label))
    // Problem for https://github.com/ShiftLeftSecurity/codescience/issues/3626 here.
    // As the type (thus, the signature) of the function node is unknown (i.e., ANY*)
    // we can't generate the correct binding with signature.
    val bindingNode = createBindingNode()
    Ast(functionTypeDeclNode).withBindsEdge(functionTypeDeclNode, bindingNode).withRefEdge(bindingNode, methodNode)
  }

}
