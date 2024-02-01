package io.joern.swiftsrc2cpg.astcreation

import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.*
import io.joern.swiftsrc2cpg.passes.Defines
import io.joern.x2cpg
import io.joern.x2cpg.Ast
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.utils.NodeBuilders.newMethodReturnNode
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.Operators

trait AstNodeBuilder(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def setOrderExplicitly(ast: Ast, order: Int): Unit = {
    ast.root.foreach { case expr: ExpressionNew => expr.order = order }
  }

  protected def codeOf(node: NewNode): String = node match {
    case astNodeNew: AstNodeNew => astNodeNew.code
    case _                      => ""
  }

  protected def createJumpTarget(switchCase: SwitchCaseSyntax | IfConfigDeclSyntax): NewJumpTarget = {
    val (switchName, switchCode) = switchCase match {
      case s: SwitchCaseSyntax =>
        val c = code(s.label)
        (c.stripSuffix(":"), c)
      case i: IfConfigDeclSyntax =>
        notHandledYet(i)
        val c = code(i.clauses.children.head)
        (c.stripSuffix(":"), c)
    }
    NewJumpTarget()
      .parserTypeName(switchCase.toString)
      .name(switchName)
      .code(switchCode)
      .lineNumber(line(switchCase))
      .columnNumber(column(switchCase))
  }

  protected def createIndexAccessCallAst(
    baseNode: NewNode,
    partNode: NewNode,
    line: Option[Integer],
    column: Option[Integer]
  ): Ast = {
    val callNode = createCallNode(
      s"${codeOf(baseNode)}[${codeOf(partNode)}]",
      Operators.indexAccess,
      DispatchTypes.STATIC_DISPATCH,
      line,
      column
    )
    val arguments = List(Ast(baseNode), Ast(partNode))
    callAst(callNode, arguments)
  }

  protected def createIndexAccessCallAst(
    baseAst: Ast,
    partAst: Ast,
    line: Option[Integer],
    column: Option[Integer],
    additionalArgsAst: Ast = Ast()
  ): Ast = {
    val callNode = createCallNode(
      s"${codeOf(baseAst.nodes.head)}[${codeOf(partAst.nodes.head)}]",
      Operators.indexAccess,
      DispatchTypes.STATIC_DISPATCH,
      line,
      column
    )
    val arguments = List(baseAst, partAst, additionalArgsAst)
    callAst(callNode, arguments)
  }

  protected def createFieldAccessCallAst(
    baseNode: NewNode,
    partNode: NewNode,
    line: Option[Integer],
    column: Option[Integer]
  ): Ast = {
    val callNode = createCallNode(
      s"${codeOf(baseNode)}.${codeOf(partNode)}",
      Operators.fieldAccess,
      DispatchTypes.STATIC_DISPATCH,
      line,
      column
    )
    val arguments = List(Ast(baseNode), Ast(partNode))
    callAst(callNode, arguments)
  }

  protected def createFieldAccessCallAst(
    baseAst: Ast,
    partNode: NewNode,
    line: Option[Integer],
    column: Option[Integer]
  ): Ast = {
    val callNode = createCallNode(
      s"${codeOf(baseAst.nodes.head)}.${codeOf(partNode)}",
      Operators.fieldAccess,
      DispatchTypes.STATIC_DISPATCH,
      line,
      column
    )
    val arguments = List(baseAst, Ast(partNode))
    callAst(callNode, arguments)
  }

  def callNode(node: SwiftNode, code: String, name: String, dispatchType: String): NewCall = {
    val fullName =
      if (dispatchType == DispatchTypes.STATIC_DISPATCH) name
      else x2cpg.Defines.DynamicCallUnknownFullName
    callNode(node, code, name, fullName, dispatchType, None, Some(Defines.Any))
  }

  private def createCallNode(
    code: String,
    callName: String,
    dispatchType: String,
    line: Option[Integer],
    column: Option[Integer]
  ): NewCall = NewCall()
    .code(code)
    .name(callName)
    .methodFullName(
      if (dispatchType == DispatchTypes.STATIC_DISPATCH) callName else x2cpg.Defines.DynamicCallUnknownFullName
    )
    .dispatchType(dispatchType)
    .lineNumber(line)
    .columnNumber(column)
    .typeFullName(Defines.Any)

  protected def createFieldIdentifierNode(
    name: String,
    line: Option[Integer],
    column: Option[Integer]
  ): NewFieldIdentifier = {
    val cleanedName = stripQuotes(name)
    NewFieldIdentifier()
      .code(cleanedName)
      .canonicalName(cleanedName)
      .lineNumber(line)
      .columnNumber(column)
  }

  protected def literalNode(node: SwiftNode, code: String, dynamicTypeOption: Option[String]): NewLiteral = {
    val typeFullName = dynamicTypeOption match {
      case Some(value) if Defines.SwiftTypes.contains(value) => value
      case _                                                 => Defines.Any
    }
    literalNode(node, code, typeFullName, dynamicTypeOption.toList)
  }

  protected def createAssignmentCallAst(
    destId: NewNode,
    sourceId: NewNode,
    code: String,
    line: Option[Integer],
    column: Option[Integer]
  ): Ast = {
    val callNode  = createCallNode(code, Operators.assignment, DispatchTypes.STATIC_DISPATCH, line, column)
    val arguments = List(Ast(destId), Ast(sourceId))
    callAst(callNode, arguments)
  }

  protected def createAssignmentCallAst(
    dest: Ast,
    source: Ast,
    code: String,
    line: Option[Integer],
    column: Option[Integer]
  ): Ast = {
    val callNode  = createCallNode(code, Operators.assignment, DispatchTypes.STATIC_DISPATCH, line, column)
    val arguments = List(dest, source)
    callAst(callNode, arguments)
  }

  protected def typeHintForThisExpression(): Seq[String] = {
    dynamicInstanceTypeStack.headOption match {
      case Some(tpe) => Seq(tpe)
      case None      => methodAstParentStack.collectFirst { case t: NewTypeDecl => t.fullName }.toSeq
    }
  }

  protected def identifierNode(node: SwiftNode, name: String): NewIdentifier = {
    val dynamicInstanceTypeOption = name match {
      case "this" | "self" | "Self" => typeHintForThisExpression().headOption
      case _                        => None
    }
    identifierNode(node, name, name, Defines.Any, dynamicInstanceTypeOption.toList)
  }

  protected def identifierNode(node: SwiftNode, name: String, dynamicTypeHints: Seq[String]): NewIdentifier = {
    identifierNode(node, name, name, Defines.Any, dynamicTypeHints)
  }

  def staticInitMethodAstAndBlock(
    initAsts: List[Ast],
    fullName: String,
    signature: Option[String],
    returnType: String,
    fileName: Option[String] = None,
    lineNumber: Option[Integer] = None,
    columnNumber: Option[Integer] = None
  ): AstAndMethod = {
    val methodNode = NewMethod()
      .name(io.joern.x2cpg.Defines.StaticInitMethodName)
      .fullName(fullName)
      .lineNumber(lineNumber)
      .columnNumber(columnNumber)
    if (signature.isDefined) {
      methodNode.signature(signature.get)
    }
    if (fileName.isDefined) {
      methodNode.filename(fileName.get)
    }
    val staticModifier = NewModifier().modifierType(ModifierTypes.STATIC)
    val body           = blockAst(NewBlock(), initAsts)
    val methodReturn   = newMethodReturnNode(returnType, None, None, None)
    AstAndMethod(methodAst(methodNode, Nil, body, methodReturn, List(staticModifier)), methodNode, body)
  }

  protected def createStaticCallNode(
    code: String,
    callName: String,
    fullName: String,
    line: Option[Integer],
    column: Option[Integer]
  ): NewCall = NewCall()
    .code(code)
    .name(callName)
    .methodFullName(fullName)
    .dispatchType(DispatchTypes.STATIC_DISPATCH)
    .signature("")
    .lineNumber(line)
    .columnNumber(column)
    .typeFullName(Defines.Any)

  protected def createFunctionTypeAndTypeDeclAst(
    node: SwiftNode,
    methodNode: NewMethod,
    methodName: String,
    methodFullName: String
  ): Ast = {
    registerType(methodFullName)

    val parentNode        = methodAstParentStack.head
    val astParentType     = parentNode.label
    val astParentFullName = parentNode.properties("FULL_NAME").toString
    val functionTypeDeclNode =
      typeDeclNode(
        node,
        methodName,
        methodFullName,
        parserResult.filename,
        methodName,
        astParentType = astParentType,
        astParentFullName = astParentFullName,
        List(Defines.Any)
      )
    Ast.storeInDiffGraph(Ast(functionTypeDeclNode), diffGraph)

    val bindingNode = NewBinding().name("").signature("")
    Ast(functionTypeDeclNode).withBindsEdge(functionTypeDeclNode, bindingNode).withRefEdge(bindingNode, methodNode)
  }

}
