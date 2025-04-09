package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.x2cpg
import io.joern.x2cpg.Ast
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.frontendspecific.jssrc2cpg.Defines
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.*

trait AstNodeBuilder(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def callNode(node: BabelNodeInfo, code: String, name: String, dispatchType: String): NewCall = {
    val fullName = dispatchType match {
      case DispatchTypes.STATIC_DISPATCH => name
      case _                             => x2cpg.Defines.DynamicCallUnknownFullName
    }
    callNode(node, code, name, fullName, dispatchType, None, Option(Defines.Any))
  }

  protected def methodReturnNode(func: BabelNodeInfo): NewMethodReturn = {
    val tpe           = typeFor(func)
    val possibleTypes = Seq(tpe)
    val typeFullName  = if (Defines.isBuiltinType(tpe)) tpe else Defines.Any
    methodReturnNode(func, typeFullName).possibleTypes(possibleTypes)
  }

  protected def jumpTargetNode(switchCase: BabelNodeInfo): NewJumpTarget = {
    val (switchName, switchCode) = if (switchCase.json("test").isNull) { ("default", "default:") }
    else { ("case", s"case ${code(switchCase.json("test"))}:") }
    val parserTypeName = switchCase.node.toString
    jumpTargetNode(switchCase, switchName, switchCode, Some(parserTypeName))
  }

  protected def createIndexAccessCallAst(baseAst: Ast, partAst: Ast, line: Option[Int], column: Option[Int]): Ast = {
    val callNode_ = callNode(
      s"${codeOf(baseAst.nodes.head)}[${codeOf(partAst.nodes.head)}]",
      Operators.indexAccess,
      DispatchTypes.STATIC_DISPATCH,
      line,
      column
    )
    val arguments = List(baseAst, partAst)
    callAst(callNode_, arguments)
  }

  protected def createFieldAccessCallAst(
    baseNode: NewNode,
    partNode: NewNode,
    line: Option[Int],
    column: Option[Int]
  ): Ast = {
    val callNode_ = callNode(
      s"${codeOf(baseNode)}.${codeOf(partNode)}",
      Operators.fieldAccess,
      DispatchTypes.STATIC_DISPATCH,
      line,
      column
    )
    val arguments = List(Ast(baseNode), Ast(partNode))
    callAst(callNode_, arguments)
  }

  protected def createFieldAccessCallAst(
    baseAst: Ast,
    partNode: NewNode,
    line: Option[Int],
    column: Option[Int]
  ): Ast = {
    val callNode_ = callNode(
      s"${codeOf(baseAst.nodes.head)}.${codeOf(partNode)}",
      Operators.fieldAccess,
      DispatchTypes.STATIC_DISPATCH,
      line,
      column
    )
    val arguments = List(baseAst, Ast(partNode))
    callAst(callNode_, arguments)
  }

  protected def createTernaryCallAst(
    testAst: Ast,
    trueAst: Ast,
    falseAst: Ast,
    line: Option[Int],
    column: Option[Int]
  ): Ast = {
    val code      = s"${codeOf(testAst.nodes.head)} ? ${codeOf(trueAst.nodes.head)} : ${codeOf(falseAst.nodes.head)}"
    val callNode_ = callNode(code, Operators.conditional, DispatchTypes.STATIC_DISPATCH, line, column)
    val arguments = List(testAst, trueAst, falseAst)
    callAst(callNode_, arguments)
  }

  protected def voidCallNode(line: Option[Int], column: Option[Int]): NewCall =
    callNode("void 0", "<operator>.void", DispatchTypes.STATIC_DISPATCH, line, column)

  protected def literalNode(node: BabelNodeInfo, code: String, dynamicTypeOption: Option[String]): NewLiteral = {
    val typeFullName = dynamicTypeOption match {
      case Some(value) if Defines.isBuiltinType(value) => value
      case _                                           => Defines.Any
    }
    literalNode(node, code, typeFullName, dynamicTypeOption.toList)
  }

  protected def createEqualsCallAst(dest: Ast, source: Ast, line: Option[Int], column: Option[Int]): Ast = {
    val code      = s"${codeOf(dest.nodes.head)} === ${codeOf(source.nodes.head)}"
    val callNode_ = callNode(code, Operators.equals, DispatchTypes.STATIC_DISPATCH, line, column)
    val arguments = List(dest, source)
    callAst(callNode_, arguments)
  }

  protected def createAssignmentCallAst(
    destId: NewNode,
    sourceId: NewNode,
    code: String,
    line: Option[Int],
    column: Option[Int]
  ): Ast = {
    val callNode_ = callNode(code, Operators.assignment, DispatchTypes.STATIC_DISPATCH, line, column)
    val arguments = List(Ast(destId), Ast(sourceId))
    callAst(callNode_, arguments)
  }

  protected def callNode(
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

  protected def createAssignmentCallAst(
    dest: Ast,
    source: Ast,
    code: String,
    line: Option[Int],
    column: Option[Int]
  ): Ast = {
    val callNode_ = callNode(code, Operators.assignment, DispatchTypes.STATIC_DISPATCH, line, column)
    val arguments = List(dest, source)
    callAst(callNode_, arguments)
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

  protected def staticCallNode(
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

  protected def templateDomNode(name: String, code: String, line: Option[Int], column: Option[Int]): NewTemplateDom =
    NewTemplateDom()
      .name(name)
      .code(code)
      .lineNumber(line)
      .columnNumber(column)

}
