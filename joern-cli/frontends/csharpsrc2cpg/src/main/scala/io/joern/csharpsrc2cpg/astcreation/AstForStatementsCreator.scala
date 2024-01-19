package io.joern.csharpsrc2cpg.astcreation

import io.circe.Json
import io.joern.csharpsrc2cpg.parser.DotNetJsonAst.{
  BinaryExpr,
  Block,
  ExpressionStatement,
  ForEachStatement,
  GlobalStatement,
  IfStatement,
  LiteralExpr,
  ThrowStatement,
  TryStatement,
  UnaryExpr
}
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

  private def astForIfStatement(ifStmt: DotNetNodeInfo): Seq[Ast] = {
    val conditionNode = createDotNetNodeInfo(ifStmt.json(ParserKeys.Condition))
    val conditionAst  = astForNode(conditionNode).headOption.getOrElse(Ast())

    val thenNode     = createDotNetNodeInfo(ifStmt.json(ParserKeys.Statement))
    val thenAst: Ast = Option(astForBlock(createDotNetNodeInfo(ifStmt.json(ParserKeys.Statement)))).getOrElse(Ast())
    val ifNode =
      controlStructureNode(ifStmt, ControlStructureTypes.IF, s"if (${conditionNode.code})")
    val elseAst = ifStmt.json(ParserKeys.Else) match
      case elseStmt: ujson.Obj => astForElseStatement(createDotNetNodeInfo(elseStmt))
      case _                   => Ast()

    Seq(controlStructureAst(ifNode, Some(conditionAst), Seq(thenAst, elseAst)))
  }

  protected def astForStatement(nodeInfo: DotNetNodeInfo): Seq[Ast] = {
    nodeInfo.node match
      case ExpressionStatement => astForExpression(nodeInfo)
      case GlobalStatement     => astForGlobalStatement(nodeInfo)
      case IfStatement         => astForIfStatement(nodeInfo)
      case ThrowStatement      => astForThrowStatement(nodeInfo)
      case TryStatement        => astForTryStatement(nodeInfo)
      case ForEachStatement    => astForForEachStatement(nodeInfo)
      case _                   => notHandledYet(nodeInfo)
  }

  private def astForForEachStatement(forEachStmt: DotNetNodeInfo): Seq[Ast] = {
    val forEachNode     = controlStructureNode(forEachStmt, ControlStructureTypes.FOR, forEachStmt.code)
    val iterableAst     = astForNode(forEachStmt.json(ParserKeys.Expression))
    val forEachBlockAst = astForBlock(createDotNetNodeInfo(forEachStmt.json(ParserKeys.Statement)))

    val identifierValue = forEachStmt.json(ParserKeys.Identifier)(ParserKeys.Value).str
    val _identifierNode =
      identifierNode(
        node = createDotNetNodeInfo(forEachStmt.json(ParserKeys.Type)),
        name = identifierValue,
        code = identifierValue,
        typeFullName = nodeTypeFullName(createDotNetNodeInfo(forEachStmt.json(ParserKeys.Type)))
      )

    val iteratorVarAst = Ast(_identifierNode)

    Seq(Ast(forEachNode).withChild(iteratorVarAst).withChildren(iterableAst).withChild(forEachBlockAst))
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

  protected def astForGlobalStatement(globalStatement: DotNetNodeInfo): Seq[Ast] = {
    astForNode(globalStatement.json(ParserKeys.Statement))
  }

}
