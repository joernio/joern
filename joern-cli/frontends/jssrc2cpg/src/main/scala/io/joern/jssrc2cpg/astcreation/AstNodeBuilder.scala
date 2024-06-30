package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.x2cpg
import io.joern.x2cpg.{Ast, ValidationMode}
import io.joern.x2cpg.frontendspecific.jssrc2cpg.Defines
import io.joern.x2cpg.utils.NodeBuilders.newMethodReturnNode
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.Operators

trait AstNodeBuilder(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>
  protected def createMethodReturnNode(func: BabelNodeInfo): NewMethodReturn = {
    val tpe           = typeFor(func)
    val possibleTypes = Seq(tpe)
    val typeFullName  = if (Defines.isBuiltinType(tpe)) tpe else Defines.Any
    newMethodReturnNode(typeFullName, line = func.lineNumber, column = func.columnNumber).possibleTypes(possibleTypes)
  }

  protected def setOrderExplicitly(ast: Ast, order: Int): Unit = {
    ast.root.foreach { case expr: ExpressionNew => expr.order = order }
  }

  protected def createJumpTarget(switchCase: BabelNodeInfo): NewJumpTarget = {
    val (switchName, switchCode) = if (switchCase.json("test").isNull) {
      ("default", "default:")
    } else {
      ("case", s"case ${code(switchCase.json("test"))}:")
    }
    NewJumpTarget()
      .parserTypeName(switchCase.node.toString)
      .name(switchName)
      .code(switchCode)
      .lineNumber(switchCase.lineNumber)
      .columnNumber(switchCase.columnNumber)
  }

  protected def codeOf(node: NewNode): String = node match {
    case astNodeNew: AstNodeNew => astNodeNew.code
    case _                      => ""
  }

  protected def createIndexAccessCallAst(
    baseNode: NewNode,
    partNode: NewNode,
    line: Option[Int],
    column: Option[Int]
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

  protected def createIndexAccessCallAst(baseAst: Ast, partAst: Ast, line: Option[Int], column: Option[Int]): Ast = {
    val callNode = createCallNode(
      s"${codeOf(baseAst.nodes.head)}[${codeOf(partAst.nodes.head)}]",
      Operators.indexAccess,
      DispatchTypes.STATIC_DISPATCH,
      line,
      column
    )
    val arguments = List(baseAst, partAst)
    callAst(callNode, arguments)
  }

  protected def createFieldAccessCallAst(
    baseNode: NewNode,
    partNode: NewNode,
    line: Option[Int],
    column: Option[Int]
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
    line: Option[Int],
    column: Option[Int]
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

  protected def createTernaryCallAst(
    testAst: Ast,
    trueAst: Ast,
    falseAst: Ast,
    line: Option[Int],
    column: Option[Int]
  ): Ast = {
    val code      = s"${codeOf(testAst.nodes.head)} ? ${codeOf(trueAst.nodes.head)} : ${codeOf(falseAst.nodes.head)}"
    val callNode  = createCallNode(code, Operators.conditional, DispatchTypes.STATIC_DISPATCH, line, column)
    val arguments = List(testAst, trueAst, falseAst)
    callAst(callNode, arguments)
  }

  def callNode(node: BabelNodeInfo, code: String, name: String, dispatchType: String): NewCall = {
    val fullName =
      if (dispatchType == DispatchTypes.STATIC_DISPATCH) name
      else x2cpg.Defines.DynamicCallUnknownFullName
    callNode(node, code, name, fullName, dispatchType, None, Option(Defines.Any))
  }

  private def createCallNode(
    code: String,
    callName: String,
    dispatchType: String,
    line: Option[Int],
    column: Option[Int]
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

  protected def createVoidCallNode(line: Option[Int], column: Option[Int]): NewCall =
    createCallNode("void 0", "<operator>.void", DispatchTypes.STATIC_DISPATCH, line, column)

  protected def createFieldIdentifierNode(name: String, line: Option[Int], column: Option[Int]): NewFieldIdentifier = {
    val cleanedName = stripQuotes(name)
    NewFieldIdentifier()
      .code(cleanedName)
      .canonicalName(cleanedName)
      .lineNumber(line)
      .columnNumber(column)
  }

  protected def literalNode(node: BabelNodeInfo, code: String, dynamicTypeOption: Option[String]): NewLiteral = {
    val typeFullName = dynamicTypeOption match {
      case Some(value) if Defines.isBuiltinType(value) => value
      case _                                           => Defines.Any
    }
    literalNode(node, code, typeFullName, dynamicTypeOption.toList)
  }

  protected def createEqualsCallAst(dest: Ast, source: Ast, line: Option[Int], column: Option[Int]): Ast = {
    val code      = s"${codeOf(dest.nodes.head)} === ${codeOf(source.nodes.head)}"
    val callNode  = createCallNode(code, Operators.equals, DispatchTypes.STATIC_DISPATCH, line, column)
    val arguments = List(dest, source)
    callAst(callNode, arguments)
  }

  protected def createAssignmentCallAst(
    destId: NewNode,
    sourceId: NewNode,
    code: String,
    line: Option[Int],
    column: Option[Int]
  ): Ast = {
    val callNode  = createCallNode(code, Operators.assignment, DispatchTypes.STATIC_DISPATCH, line, column)
    val arguments = List(Ast(destId), Ast(sourceId))
    callAst(callNode, arguments)
  }

  protected def createAssignmentCallAst(
    dest: Ast,
    source: Ast,
    code: String,
    line: Option[Int],
    column: Option[Int]
  ): Ast = {
    val callNode  = createCallNode(code, Operators.assignment, DispatchTypes.STATIC_DISPATCH, line, column)
    val arguments = List(dest, source)
    callAst(callNode, arguments)
  }

  protected def identifierNode(node: BabelNodeInfo, name: String): NewIdentifier = {
    val dynamicInstanceTypeOption = name match {
      case "this"    => typeHintForThisExpression(Option(node)).headOption
      case "console" => Option(Defines.Console)
      case "Math"    => Option(Defines.Math)
      case _         => None
    }
    identifierNode(node, name, name, Defines.Any, dynamicInstanceTypeOption.toList)
  }

  protected def identifierNode(node: BabelNodeInfo, name: String, dynamicTypeHints: Seq[String]): NewIdentifier = {
    identifierNode(node, name, name, Defines.Any, dynamicTypeHints)
  }

  protected def createStaticCallNode(
    code: String,
    callName: String,
    fullName: String,
    line: Option[Int],
    column: Option[Int]
  ): NewCall = NewCall()
    .code(code)
    .name(callName)
    .methodFullName(fullName)
    .dispatchType(DispatchTypes.STATIC_DISPATCH)
    .signature("")
    .lineNumber(line)
    .columnNumber(column)
    .typeFullName(Defines.Any)

  protected def createTemplateDomNode(
    name: String,
    code: String,
    line: Option[Int],
    column: Option[Int]
  ): NewTemplateDom =
    NewTemplateDom()
      .name(name)
      .code(code)
      .lineNumber(line)
      .columnNumber(column)

  protected def createBlockNode(node: BabelNodeInfo, customCode: Option[String] = None): NewBlock =
    NewBlock()
      .typeFullName(Defines.Any)
      .code(customCode.getOrElse(node.code))
      .lineNumber(node.lineNumber)
      .columnNumber(node.columnNumber)

  protected def createFunctionTypeAndTypeDeclAst(
    node: BabelNodeInfo,
    methodNode: NewMethod,
    parentNode: NewNode,
    methodName: String,
    methodFullName: String,
    filename: String
  ): Ast = {
    registerType(methodFullName)

    val astParentType     = parentNode.label
    val astParentFullName = parentNode.properties("FULL_NAME").toString
    val functionTypeDeclNode =
      typeDeclNode(
        node,
        methodName,
        methodFullName,
        filename,
        methodName,
        astParentType = astParentType,
        astParentFullName = astParentFullName,
        List(Defines.Any)
      )

    // Problem for https://github.com/ShiftLeftSecurity/codescience/issues/3626 here.
    // As the type (thus, the signature) of the function node is unknown (i.e., ANY*)
    // we can't generate the correct binding with signature.
    val bindingNode = NewBinding().name("").signature("")
    Ast(functionTypeDeclNode).withBindsEdge(functionTypeDeclNode, bindingNode).withRefEdge(bindingNode, methodNode)
  }

}
