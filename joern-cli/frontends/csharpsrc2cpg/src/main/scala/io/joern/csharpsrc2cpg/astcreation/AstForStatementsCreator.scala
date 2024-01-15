package io.joern.csharpsrc2cpg.astcreation

import io.joern.csharpsrc2cpg.CSharpOperators
import io.joern.csharpsrc2cpg.parser.DotNetJsonAst.{ExpressionStatement, GlobalStatement, ThrowStatement}
import io.joern.csharpsrc2cpg.parser.{DotNetNodeInfo, ParserKeys}
import io.joern.x2cpg.{Ast, ValidationMode}

trait AstForStatementsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  def astForStatement(nodeInfo: DotNetNodeInfo): Seq[Ast] = {
    nodeInfo.node match
      case ExpressionStatement => astForExpression(nodeInfo)
      case GlobalStatement     => astForGlobalStatement(nodeInfo)
      case ThrowStatement      => astForThrowStatment(nodeInfo)
      case _                   => notHandledYet(nodeInfo)
  }

  def astForGlobalStatement(globalStatement: DotNetNodeInfo): Seq[Ast] = {
    astForNode(globalStatement.json(ParserKeys.Statement))
  }

  def astForThrowStatment(throwStmt: DotNetNodeInfo): Seq[Ast] = {
    val expr = createDotNetNodeInfo(throwStmt.json(ParserKeys.Expression))
    val args = astForNode(expr)
    val throwCall = createCallNodeForOperator(
      throwStmt,
      CSharpOperators.throws,
      typeFullName = Option(getTypeFullNameFromAstNode(args))
    )
    Seq(callAst(throwCall, args))
  }

}
