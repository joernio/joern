package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.parser.ParserAst.*
import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.gosrc2cpg.utils.Operator
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.{ExpressionNew, NewIdentifier, NewLocal}
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, Operators}
import ujson.Value

import scala.util.Try

trait AstForStatementsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>
  def astForBlockStatement(blockStmt: ParserNodeInfo, order: Int = -1): Ast = {
    val newBlockNode = blockNode(blockStmt, Defines.empty, Defines.voidTypeName).order(order).argumentIndex(order)
    scope.pushNewScope(newBlockNode)
    var currOrder = 1
    val childAsts =
      blockStmt.json(ParserKeys.List).arrOpt.getOrElse(List()).arr.flatMap { parserJsonValue =>
        val parserNode = createParserNodeInfo(parserJsonValue)
        val r          = astsForStatement(parserNode, currOrder)
        currOrder = currOrder + r.length
        r
      }
    scope.popScope()
    blockAst(newBlockNode, childAsts.toList)
  }

  protected def astsForStatement(statementJson: Value): Seq[Ast] = {
    astsForStatement(createParserNodeInfo(statementJson))
  }
  protected final def astsForStatement(statement: ParserNodeInfo, argIndex: Int = -1): Seq[Ast] = {
    statement.node match {
      case AssignStmt     => astForAssignStatement(statement)
      case BranchStmt     => Seq(astForBranchStatement(statement))
      case BlockStmt      => Seq(astForBlockStatement(statement, argIndex))
      case CaseClause     => astForCaseClause(statement)
      case DeclStmt       => astForNode(statement.json(ParserKeys.Decl))
      case ExprStmt       => astsForExpression(createParserNodeInfo(statement.json(ParserKeys.X)))
      case ForStmt        => Seq(astForForStatement(statement))
      case IfStmt         => astForIfStatement(statement)
      case IncDecStmt     => Seq(astForIncDecStatement(statement))
      case RangeStmt      => Seq(astForRangeStatement(statement))
      case SwitchStmt     => Seq(astForSwitchStatement(statement))
      case TypeSwitchStmt => Seq(astForTypeSwitchStatement(statement))
      case ReturnStmt     => Seq(astForReturnStatement(statement))
      case Unknown        => Seq(Ast())
      case _: BaseStmt    => Seq(Ast())
      case _              => astForNode(statement.json)
    }
  }

  private def astForReturnStatement(returnStmt: ParserNodeInfo): Ast = {
    // TODO: Need to handle the tuple return node handling
    val cpgReturn = returnNode(returnStmt, returnStmt.code)
    val expast = returnStmt
      .json(ParserKeys.Results)
      .arrOpt
      .getOrElse(Seq.empty)
      .flatMap(x => astForNode(x))
      .toSeq

    returnAst(cpgReturn, expast)
  }

  private def astForAssignStatement(assignStmt: ParserNodeInfo): Seq[Ast] = {
    assignStmt.json(ParserKeys.Tok).value match {
      case "="   => astForOnlyAssignmentOperator(assignStmt, Operators.assignment)
      case ":="  => astForDeclrationAssignment(assignStmt, Operators.assignment)
      case "*="  => astForOnlyAssignmentOperator(assignStmt, Operators.assignmentMultiplication)
      case "/="  => astForOnlyAssignmentOperator(assignStmt, Operators.assignmentDivision)
      case "%="  => astForOnlyAssignmentOperator(assignStmt, Operators.assignmentModulo)
      case "+="  => astForOnlyAssignmentOperator(assignStmt, Operators.assignmentPlus)
      case "-="  => astForOnlyAssignmentOperator(assignStmt, Operators.assignmentMinus)
      case "<<=" => astForOnlyAssignmentOperator(assignStmt, Operators.assignmentShiftLeft)
      case ">>=" => astForOnlyAssignmentOperator(assignStmt, Operators.assignmentArithmeticShiftRight)
      case "&="  => astForOnlyAssignmentOperator(assignStmt, Operators.assignmentAnd)
      case "^="  => astForOnlyAssignmentOperator(assignStmt, Operators.assignmentXor)
      case "|="  => astForOnlyAssignmentOperator(assignStmt, Operators.assignmentOr)
      case _     => astForOnlyAssignmentOperator(assignStmt, Operator.unknown)
    }
  }

  private def astForDeclrationAssignment(assignStmt: ParserNodeInfo, op: String): Seq[Ast] = {
    val (assCallAsts, localAsts) =
      (assignStmt.json(ParserKeys.Lhs).arr.toList zip assignStmt.json(ParserKeys.Rhs).arr.toList)
        .map { case (lhs, rhs) => (createParserNodeInfo(lhs), createParserNodeInfo(rhs)) }
        .map { case (lhsParserNode, rhsParserNode) =>
          astForAssignmentCallNode(lhsParserNode, rhsParserNode, None, assignStmt.code)
        }
        .unzip
    localAsts ++: assCallAsts
  }

  private def astForOnlyAssignmentOperator(assignStmt: ParserNodeInfo, op: String): Seq[Ast] = {
    val rhsAst = assignStmt
      .json(ParserKeys.Rhs)
      .arr
      .map(createParserNodeInfo)
      .flatMap(astForBooleanLiteral)
    val typeFullName = Some(getTypeFullNameFromAstNode(rhsAst.toSeq))
    val lhsAst = assignStmt
      .json(ParserKeys.Lhs)
      .arr
      .flatMap(astForNode)

    val arguments = lhsAst ++: rhsAst
    val cNode     = callNode(assignStmt, assignStmt.code, op, op, DispatchTypes.STATIC_DISPATCH, None, typeFullName)
    val callAst_  = Seq(callAst(cNode, arguments.toSeq))
    callAst_
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

  private def astForConditionExpression(condStmt: ParserNodeInfo, explicitArgumentIndex: Option[Int] = None): Ast = {
    val ast = condStmt.node match {
      case ParenExpr   => astForNode(condStmt.json(ParserKeys.X)).headOption.getOrElse(Ast())
      case _: BaseExpr => astsForExpression(condStmt).headOption.getOrElse(Ast())
      case _           => astsForStatement(condStmt).headOption.getOrElse(Ast())
    }
    explicitArgumentIndex.foreach { i =>
      ast.root.foreach { case expr: ExpressionNew => expr.argumentIndex = i }
    }
    ast
  }

  private def astForIfStatement(ifStmt: ParserNodeInfo): Seq[Ast] = {
    // handle init code before condition in if;
    val initParserNode = nullSafeCreateParserNodeInfo(ifStmt.json.obj.get(ParserKeys.Init))
    val initAstBlock   = blockNode(ifStmt, Defines.empty, Defines.voidTypeName)
    scope.pushNewScope(initAstBlock)
    val initAst = blockAst(initAstBlock, astsForStatement(initParserNode, 1).toList)
    scope.popScope()

    val conditionParserNode = createParserNodeInfo(ifStmt.json(ParserKeys.Cond))
    val conditionAst        = astForConditionExpression(conditionParserNode)
    val ifCode              = s"if ${conditionParserNode.code}"

    val thenAst = astForBlockStatement(createParserNodeInfo(ifStmt.json(ParserKeys.Body)))

    val elseNode = Try(ifStmt.json(ParserKeys.Else)).toOption.map(createParserNodeInfo)
    val elseAst = elseNode match {
      case Some(elseStmt) if elseStmt.node == BlockStmt =>
        Some(astForBlockStatement(elseStmt))
      case Some(elseStmt) =>
        val elseBlock = blockNode(elseStmt, Defines.empty, Defines.voidTypeName)
        scope.pushNewScope(elseBlock)
        val statementAsts = astsForStatement(elseStmt)
        setArgumentIndices(statementAsts)
        scope.popScope()
        Some(blockAst(elseBlock, statementAsts.toList))
      case _ => None
    }
    Seq(initAst, ifThenElseAst(ifStmt, Some(conditionAst), thenAst, elseAst, Some(ifCode)))
  }

  private def astForSwitchStatement(switchStmt: ParserNodeInfo): Ast = {
    val conditionParserNode = Try(createParserNodeInfo(switchStmt.json(ParserKeys.Tag)))
    val (code, conditionAst) = conditionParserNode.toOption match {
      case Some(node) => (node.code, Some(astForConditionExpression(node)))
      case _          => ("", None)
    }
    val stmtAsts = astsForStatement(createParserNodeInfo(switchStmt.json(ParserKeys.Body)))
    switchAst(switchStmt, conditionAst, stmtAsts, Some(s"switch $code"))
  }

  private def astForTypeSwitchStatement(typeSwitchStmt: ParserNodeInfo): Ast = {
    val conditionParserNode = Try(createParserNodeInfo(typeSwitchStmt.json(ParserKeys.Assign)))
    val (code, conditionAst) = conditionParserNode.toOption match {
      case Some(node) => (node.code, astForNode(node))
      case _          => ("", Seq.empty)
    }
    val stmtAsts = astsForStatement(createParserNodeInfo(typeSwitchStmt.json(ParserKeys.Body)))
    val id = conditionAst
      .flatMap(_.root)
      .collectFirst {
        case x: NewIdentifier => identifierNode(conditionParserNode.get, x.name, x.code, x.typeFullName)
        case x: NewLocal      => identifierNode(conditionParserNode.get, x.name, x.code, x.typeFullName)
      }
      .get
    val identifier = Ast(id)
    val isOp =
      callNode(conditionParserNode.get, s"${id.name}.(type)", Operators.is, Operators.is, DispatchTypes.STATIC_DISPATCH)
    val condition = Option(callAst(isOp, Seq(identifier)))

    val newStmtAst = stmtAsts // TODO: Push conditionAst to the front of the block
    switchAst(typeSwitchStmt, condition, newStmtAst, Some(s"switch $code"))
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

  private def astForForStatement(forStmt: ParserNodeInfo): Ast = {
    val initParserNode = nullSafeCreateParserNodeInfo(forStmt.json.obj.get(ParserKeys.Init))
    val condParserNode = nullSafeCreateParserNodeInfo(forStmt.json.obj.get(ParserKeys.Cond))
    val iterParserNode = nullSafeCreateParserNodeInfo(forStmt.json.obj.get(ParserKeys.Post))
    val code           = s"for ${initParserNode.code};${condParserNode.code};${iterParserNode.code}"

    val initAstBlock = blockNode(forStmt, Defines.empty, Defines.voidTypeName)
    scope.pushNewScope(initAstBlock)
    val initAst = blockAst(initAstBlock, astsForStatement(initParserNode, 1).toList)
    scope.popScope()

    val compareAst = astForConditionExpression(condParserNode, Some(2))
    val updateAst  = astsForStatement(iterParserNode, 3)
    val bodyAsts   = astsForStatement(createParserNodeInfo(forStmt.json(ParserKeys.Body)), 4)
    forAst(forStmt, Seq(), Seq(initAst), Seq(compareAst), updateAst, bodyAsts, Some(code))

  }

  private def astForRangeStatement(rangeStmt: ParserNodeInfo): Ast = {
    rangeStmt.json.obj.contains(ParserKeys.Key) match {
      case true =>
        val keyParserNode  = createParserNodeInfo(rangeStmt.json(ParserKeys.Key))
        val declParserNode = createParserNodeInfo(keyParserNode.json(ParserKeys.Obj)(ParserKeys.Decl))
        val code           = s"for ${declParserNode.code}"
        val declAst        = astsForStatement(declParserNode)
        val initAst        = astForNode(rangeStmt.json(ParserKeys.X))
        val stmtAst        = astsForStatement(rangeStmt.json(ParserKeys.Body))
        forAst(rangeStmt, Nil, initAst, Nil, declAst, stmtAst, Some(code))
      case false =>
        val initAst = astForNode(rangeStmt.json(ParserKeys.X))
        val code    = s"for range ${createParserNodeInfo(rangeStmt.json(ParserKeys.X)).code}"
        val stmtAst = astsForStatement(rangeStmt.json(ParserKeys.Body))
        forAst(rangeStmt, Nil, initAst, Nil, Nil, stmtAst, Some(code))
    }
  }

  private def astForBranchStatement(branchStmt: ParserNodeInfo): Ast = {
    branchStmt.json(ParserKeys.Tok).str match {
      case "break"    => breakAst(branchStmt, branchStmt.code)
      case "continue" => continueAst(branchStmt, branchStmt.code)
      case "goto"     =>
        // To update the cache of parserNode with the labelled statement
        Try(createParserNodeInfo(branchStmt.json(ParserKeys.Label)(ParserKeys.Obj)(ParserKeys.Decl)))
        val labelName = branchStmt.json(ParserKeys.Label)(ParserKeys.Name).str
        gotoAst(branchStmt, branchStmt.code, labelName)
      case "fallthrough" => // TODO handling for FALLTHROUGH
        Ast()
    }
  }
}
