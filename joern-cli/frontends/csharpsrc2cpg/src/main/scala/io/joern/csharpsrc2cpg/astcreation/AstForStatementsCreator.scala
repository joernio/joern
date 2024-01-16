package io.joern.csharpsrc2cpg.astcreation

import io.circe.Json
import io.joern.csharpsrc2cpg.parser.DotNetJsonAst.{
  BinaryExpr,
  Block,
  GlobalStatement,
  IfStatement,
  LiteralExpr,
  UnaryExpr
}
import io.joern.csharpsrc2cpg.parser.{DotNetNodeInfo, ParserKeys}
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.nodes.ControlStructure

import scala.util.{Failure, Success, Try}

trait AstForStatementsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>
  def astForStatement(statement: ujson.Value): Seq[Ast] = {
    astForStatement(createDotNetNodeInfo(statement))
  }
  def astForStatement(statement: DotNetNodeInfo): Seq[Ast] = {
    statement.node match
      case IfStatement     => astForIfStatement(statement)
      case GlobalStatement => astForGlobalStatement(statement)
      case _               => notHandledYet(statement)
  }

  private def astForElseStatement(elseParserNode: DotNetNodeInfo): Ast = {
    val elseNode = controlStructureNode(elseParserNode, ControlStructureTypes.ELSE, "else")

    Option(elseParserNode.json(ParserKeys.Statement)) match
      case Some(elseStmt: ujson.Value) if createDotNetNodeInfo(elseStmt).node == Block =>
        val blockAst: Ast = astForBlock(createDotNetNodeInfo(elseParserNode.json(ParserKeys.Statement)))
        Ast(elseNode).withChild(blockAst)
      case Some(elseStmt) =>
        astForNode(createDotNetNodeInfo(elseParserNode.json(ParserKeys.Statement))).headOption.getOrElse(Ast())
      case None => Ast()

  }
  private def astForIfStatement(ifStmt: DotNetNodeInfo): Seq[Ast] = {
    val conditionNode = createDotNetNodeInfo(ifStmt.json(ParserKeys.Condition))
    val conditionAst  = astForConditionExpression(conditionNode)

    val thenNode     = createDotNetNodeInfo(ifStmt.json(ParserKeys.Statement))
    val thenAst: Ast = Option(astForBlock(createDotNetNodeInfo(ifStmt.json(ParserKeys.Statement)))).getOrElse(Ast())
    val ifNode =
      controlStructureNode(ifStmt, ControlStructureTypes.IF, s"if (${conditionNode.code})")
    val elseAst = ifStmt.json(ParserKeys.Else) match
      case elseStmt: ujson.Obj => astForElseStatement(createDotNetNodeInfo(elseStmt))
      case _                   => Ast()

    Seq(controlStructureAst(ifNode, Some(conditionAst), Seq(thenAst, elseAst)))
  }

  private def astForConditionExpression(conditionNode: DotNetNodeInfo): Ast = {
    conditionNode.node match
      case _: BinaryExpr =>
        astForBinaryExpression(conditionNode).headOption.getOrElse(Ast())
      case _: LiteralExpr =>
        astForLiteralExpression(conditionNode).headOption.getOrElse(Ast())
      case _: UnaryExpr => astForUnaryExpression(conditionNode).headOption.getOrElse(Ast())
      case _            => notHandledYet(conditionNode).headOption.getOrElse(Ast())
  }

  private def astForGlobalStatement(globalStatement: DotNetNodeInfo): Seq[Ast] = {
    astForNode(globalStatement.json(ParserKeys.Statement))
  }
}
