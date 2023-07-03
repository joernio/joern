package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.parser.ParserAst._
import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.gosrc2cpg.utils.Operator
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, Operators}
import ujson.Value

import scala.util.Try

trait AstForStatementsCreator { this: AstCreator =>
  def astForBlockStatement(blockStmt: ParserNodeInfo): Seq[Ast] = {

    val newBlockNode = blockNode(blockStmt)
    scope.pushNewScope(newBlockNode)
    val childAsts =
      blockStmt.json(ParserKeys.List).arrOpt.getOrElse(List()).arr.map(createParserNodeInfo).flatMap { parserNode =>
        parserNode.node match {
          case DeclStmt   => astForDeclStatement(parserNode)
          case AssignStmt => astForAssignStatement(parserNode)
          case IncDecStmt => astForIncDecStatement(parserNode)
          case IfStmt => Seq(astForIfStatement(parserNode))
          case _          => Seq()
        }
      }
    scope.popScope()
    Seq(blockAst(newBlockNode, childAsts.toList))
  }

  private def astForDeclStatement(declStmt: ParserNodeInfo): Seq[Ast] = {
    val nodeInfo = createParserNodeInfo(declStmt.json(ParserKeys.Decl))
    nodeInfo.node match {
      case GenDecl => astForGenDecl(nodeInfo)
    }
  }

  private def astForAssignStatement(assignStmt: ParserNodeInfo): Seq[Ast] = {
    val op = assignStmt.json(ParserKeys.Tok).value match {
      case "="   => Operators.assignment
      case ":="  => Operators.assignment
      case "*="  => Operators.assignmentMultiplication
      case "/="  => Operators.assignmentDivision
      case "%="  => Operators.assignmentModulo
      case "+="  => Operators.assignmentPlus
      case "-="  => Operators.assignmentMinus
      case "<<=" => Operators.assignmentShiftLeft
      case ">>=" => Operators.assignmentArithmeticShiftRight
      case "&="  => Operators.assignmentAnd
      case "^="  => Operators.assignmentXor
      case "|="  => Operators.assignmentOr
      case _     => Operator.unknown
    }
    val cNode = callNode(assignStmt, assignStmt.code, op, op, DispatchTypes.STATIC_DISPATCH)

    // create corresponding local node as this is known as short variable declaration operator
    val localNodesIfIntialized =
      if (assignStmt.json(ParserKeys.Tok).value == ":=") createLocalNodeForShortVariableDeclaration(assignStmt)
      else Seq()
    val arguments = assignStmt.json(ParserKeys.Lhs).arr.flatMap(astForNode).toList ::: assignStmt
      .json(ParserKeys.Rhs)
      .arr
      .flatMap(astForNode)
      .toList
    val callAst_ = Seq(callAst(cNode, arguments))
    localNodesIfIntialized ++: callAst_
  }

  private def astForIncDecStatement(incDecStatement: ParserNodeInfo): Seq[Ast] = {
    val op = incDecStatement.json(ParserKeys.Tok).value match {
      case "++" => Operators.postIncrement
      case "--" => Operators.postDecrement
      case _    => Operator.unknown
    }
    val cNode   = callNode(incDecStatement, incDecStatement.code, op, op, DispatchTypes.STATIC_DISPATCH)
    val operand = astForNode(incDecStatement.json(ParserKeys.X))
    Seq(callAst(cNode, (operand)))
  }

  def createLocalNodeForShortVariableDeclaration(assignStmt: ParserNodeInfo): Seq[Ast] = {

    val localNodes = (assignStmt.json(ParserKeys.Lhs).arr zip assignStmt.json(ParserKeys.Rhs).arr)
      .map { case (lhs, rhs) => (createParserNodeInfo(lhs), createParserNodeInfo(rhs)) }
      .map { case (localParserNode, rhsParserNode) =>
        val name = localParserNode.json(ParserKeys.Name).str
        val typ  = getTypeOfToken(rhsParserNode)
        val node = localNode(localParserNode, name, localParserNode.code, typ)
        scope.addToScope(name, (node, typ))
        node
      }
    Seq(Ast(localNodes))
  }

  def astForConditionExpression(condStmt: ParserNodeInfo) : Ast = {
    condStmt.node match {
      case ParenExpr => astForNode(condStmt.json(ParserKeys.X)).head
      case _ => Ast()
    }
  }

  def astForIfStatement(ifStmt : ParserNodeInfo): Ast = {

    val conditionParserNode = createParserNodeInfo(ifStmt.json("Cond"))
    val conditionAst = astForConditionExpression(conditionParserNode)

    val ifNode = controlStructureNode(ifStmt, ControlStructureTypes.IF, s"if ${conditionParserNode.code}")

    val thenAst = astForBlockStatement(createParserNodeInfo(ifStmt.json(ParserKeys.Body))).head

    val elseAst = Try(ifStmt.json("Else")).toOption match {
      case Some(elseStmt) =>
        val elseParserNode = createParserNodeInfo(elseStmt)
        val elseNode = controlStructureNode(elseParserNode, ControlStructureTypes.ELSE, "else")
        val elseAst = astForBlockStatement(elseParserNode).head
        Ast(elseNode).withChild(elseAst)
      case _ => Ast()
    }
    controlStructureAst(ifNode, Some(conditionAst), Seq(thenAst, elseAst))
  }

}
