package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.parser.ParserAst._
import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.gosrc2cpg.utils.Operator
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, Operators}

import scala.annotation.tailrec
import scala.util.Try

trait AstForStatementsCreator { this: AstCreator =>
  def astForBlockStatement(blockStmt: ParserNodeInfo, order: Int = -1): Ast = {
    val newBlockNode = blockNode(blockStmt, Defines.empty, Defines.voidTypeName).order(order).argumentIndex(order)
    scope.pushNewScope(newBlockNode)
    var currOrder = 1
    val childAsts =
      blockStmt.json(ParserKeys.List).arrOpt.getOrElse(List()).arr.map(createParserNodeInfo).flatMap { parserNode =>
        val r = astsForStatement(parserNode, currOrder)
        currOrder = currOrder + r.length
        r
      }
    scope.popScope()
    blockAst(newBlockNode, childAsts.toList)
  }

  @tailrec
  private def astsForStatement(statement: ParserNodeInfo, argIndex: Int = -1): Seq[Ast] = {
    statement.node match {
      case AssignStmt     => astForAssignStatement(statement)
      case BlockStmt      => Seq(astForBlockStatement(statement, argIndex))
      case CaseClause     => astForCaseClause(statement)
      case DeclStmt       => astForDeclStatement(statement)
      case ExprStmt       => astsForStatement(createParserNodeInfo(statement.json(ParserKeys.X)))
      case IfStmt         => Seq(astForIfStatement(statement))
      case IncDecStmt     => Seq(astForIncDecStatement(statement))
      case SwitchStmt     => Seq(astForSwitchStatement(statement))
      case TypeAssertExpr => astForNode(statement.json(ParserKeys.X))
      case TypeSwitchStmt => Seq(astForTypeSwitchStatement(statement))
      case _              => astForNode(statement.json)
    }
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

  private def astForIncDecStatement(incDecStatement: ParserNodeInfo): Ast = {
    val op = incDecStatement.json(ParserKeys.Tok).value match {
      case "++" => Operators.postIncrement
      case "--" => Operators.postDecrement
      case _    => Operator.unknown
    }
    val cNode   = callNode(incDecStatement, incDecStatement.code, op, op, DispatchTypes.STATIC_DISPATCH)
    val operand = astForNode(incDecStatement.json(ParserKeys.X))
    callAst(cNode, operand)
  }

  private def createLocalNodeForShortVariableDeclaration(assignStmt: ParserNodeInfo): Seq[Ast] = {

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

  private def astForConditionExpression(condStmt: ParserNodeInfo): Ast = {
    condStmt.node match {
      case ParenExpr => astForNode(condStmt.json(ParserKeys.X)).head
      case _         => astsForStatement(condStmt).headOption.getOrElse(Ast())
    }
  }

  private def astForIfStatement(ifStmt: ParserNodeInfo): Ast = {

    val conditionParserNode = createParserNodeInfo(ifStmt.json(ParserKeys.Cond))
    val conditionAst        = astForConditionExpression(conditionParserNode)

    val ifNode = controlStructureNode(ifStmt, ControlStructureTypes.IF, s"if ${conditionParserNode.code}")

    val thenAst = astForBlockStatement(createParserNodeInfo(ifStmt.json(ParserKeys.Body)))

    val elseAst = Try(ifStmt.json(ParserKeys.Else)).toOption match {
      case Some(elseStmt) if createParserNodeInfo(elseStmt).node == BlockStmt =>
        val elseParserNode = createParserNodeInfo(elseStmt)
        val elseNode       = controlStructureNode(elseParserNode, ControlStructureTypes.ELSE, "else")
        val elseAst        = astForBlockStatement(elseParserNode)
        Ast(elseNode).withChild(elseAst)
      case Some(elseStmt) =>
        val elseParserNode = createParserNodeInfo(elseStmt)
        val elseNode       = controlStructureNode(elseParserNode, ControlStructureTypes.ELSE, "else")
        val elseBlock      = blockNode(elseParserNode, Defines.empty, Defines.voidTypeName)
        scope.pushNewScope(elseBlock)
        val a = astsForStatement(elseParserNode)
        setArgumentIndices(a)
        scope.popScope()
        Ast(elseNode).withChild(blockAst(elseBlock, a.toList))
      case _ => Ast()
    }
    controlStructureAst(ifNode, Some(conditionAst), Seq(thenAst, elseAst))
  }

  private def astForSwitchStatement(switchStmt: ParserNodeInfo): Ast = {

    val conditionParserNode = Try(createParserNodeInfo(switchStmt.json(ParserKeys.Tag)))
    val (code, conditionAst) = conditionParserNode.toOption match {
      case Some(node) => (node.code, Some(astForConditionExpression(node)))
      case _          => ("", None)
    }
    val switchNode = controlStructureNode(switchStmt, ControlStructureTypes.SWITCH, s"switch $code")
    val stmtAsts   = astsForStatement(createParserNodeInfo(switchStmt.json(ParserKeys.Body)))
    controlStructureAst(switchNode, conditionAst, stmtAsts)
  }

  private def astForTypeSwitchStatement(typeSwitchStmt: ParserNodeInfo): Ast = {

    val conditionParserNode = Try(createParserNodeInfo(typeSwitchStmt.json(ParserKeys.Assign)))
    val (code, conditionAst) = conditionParserNode.toOption match {
      case Some(node) => (node.code, Some(astForConditionExpression(node)))
      case _          => ("", None)
    }
    val switchNode = controlStructureNode(typeSwitchStmt, ControlStructureTypes.SWITCH, s"switch $code")
    val stmtAsts   = astsForStatement(createParserNodeInfo(typeSwitchStmt.json(ParserKeys.Body)))
    controlStructureAst(switchNode, conditionAst, stmtAsts)
  }

  private def astForCaseClause(caseStmt: ParserNodeInfo): Seq[Ast] = {
    val caseClauseAst = caseStmt.json(ParserKeys.List).arrOpt match {
      case Some(caseConditionList) =>
        caseConditionList.flatMap { caseConditionNode =>
          val caseConditionParserNode = createParserNodeInfo(caseConditionNode)
          val jumpTarget              = jumpTargetNode(caseStmt, "case", s"case ${caseConditionParserNode.code}")
          val labelAsts               = astForNode(caseConditionNode).toList
          Ast(jumpTarget) :: labelAsts
        }
      case _ =>
        val target = jumpTargetNode(caseStmt, "default", "default")
        Seq(Ast(target))
    }

    val caseBodyAst = caseStmt.json(ParserKeys.Body).arr.map(createParserNodeInfo).flatMap(astsForStatement(_)).toList
    caseClauseAst ++: caseBodyAst
  }
}
