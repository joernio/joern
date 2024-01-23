package io.joern.csharpsrc2cpg.astcreation

import io.circe.Json
import io.joern.csharpsrc2cpg.parser.DotNetJsonAst.{
  BinaryExpr,
  Block,
  DoStatement,
  ExpressionStatement,
  ForEachStatement,
  ForStatement,
  GlobalStatement,
  IfStatement,
  LiteralExpr,
  ThrowStatement,
  TryStatement,
  UnaryExpr,
  WhileStatement
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
      case ExpressionStatement => astForExpressionStatement(nodeInfo)
      case GlobalStatement     => astForGlobalStatement(nodeInfo)
      case IfStatement         => astForIfStatement(nodeInfo)
      case ThrowStatement      => astForThrowStatement(nodeInfo)
      case TryStatement        => astForTryStatement(nodeInfo)
      case ForEachStatement    => astForForEachStatement(nodeInfo)
      case ForStatement        => astForForStatement(nodeInfo)
      case DoStatement         => astForDoStatement(nodeInfo)
      case WhileStatement      => astForWhileStatement(nodeInfo)
      case _                   => notHandledYet(nodeInfo)
  }

  private def astForWhileStatement(whileStmt: DotNetNodeInfo): Seq[Ast] = {
    val whileBlock    = createDotNetNodeInfo(whileStmt.json(ParserKeys.Statement))
    val whileBlockAst = astForBlock(whileBlock)

    val condition    = createDotNetNodeInfo(whileStmt.json(ParserKeys.Condition))
    val conditionAst = astForNode(condition)

    val code = s"while (${condition.code})"

    val whileNode = controlStructureNode(whileStmt, ControlStructureTypes.WHILE, code)

    Seq(Ast(whileNode).withChild(whileBlockAst).withChildren(conditionAst))
  }

  private def astForDoStatement(doStmt: DotNetNodeInfo): Seq[Ast] = {
    val doBlock    = createDotNetNodeInfo(doStmt.json(ParserKeys.Statement))
    val doBlockAst = astForBlock(doBlock)

    val condition    = createDotNetNodeInfo(doStmt.json(ParserKeys.Condition))
    val conditionAst = astForNode(condition)

    val code        = s"do {...} while (${condition.code})"
    val doBlockNode = controlStructureNode(doStmt, ControlStructureTypes.DO, code)

    Seq(Ast(doBlockNode).withChild(doBlockAst).withChildren(conditionAst))
  }

  private def astForForStatement(forStmt: DotNetNodeInfo): Seq[Ast] = {
    val initNode        = nullSafeCreateParserNodeInfo(forStmt.json.obj.get(ParserKeys.Declaration))
    val conditionNode   = nullSafeCreateParserNodeInfo(forStmt.json.obj.get(ParserKeys.Condition))
    val incrementorNode = nullSafeCreateParserNodeInfo(forStmt.json(ParserKeys.Incrementors).arr.headOption)

    val forBodyAst = astForBlock(createDotNetNodeInfo(forStmt.json(ParserKeys.Statement)))

    val code = s"for (${initNode.code};${conditionNode.code};${incrementorNode.code})"
    val forNode =
      controlStructureNode(forStmt, ControlStructureTypes.FOR, code);

    val initNodeAst    = astForNode(initNode)
    val conditionAst   = astForNode(conditionNode)
    val incrementorAst = astForNode(incrementorNode)

    val _forAst = Ast(forNode)
      .withChildren(initNodeAst)
      .withChildren(conditionAst)
      .withChildren(incrementorAst)
      .withChild(forBodyAst)
      .withConditionEdges(forNode, conditionAst.flatMap(_.root).toList)

    Seq(_forAst)
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
