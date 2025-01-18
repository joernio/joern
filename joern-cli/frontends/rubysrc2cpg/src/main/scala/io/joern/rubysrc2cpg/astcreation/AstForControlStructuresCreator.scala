package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.{
  ArrayLiteral,
  ArrayPattern,
  BinaryExpression,
  BreakExpression,
  CaseExpression,
  ControlFlowStatement,
  DoWhileExpression,
  DummyAst,
  DynamicLiteral,
  ElseClause,
  ForExpression,
  IfExpression,
  InClause,
  IndexAccess,
  MatchVariable,
  MemberCall,
  NextExpression,
  OperatorAssignment,
  RescueExpression,
  RubyExpression,
  SimpleIdentifier,
  SimpleObjectInstantiation,
  SingleAssignment,
  SplattingRubyNode,
  StatementList,
  StaticLiteral,
  UnaryExpression,
  Unknown,
  UnlessExpression,
  UntilExpression,
  WhenClause,
  WhileExpression
}
import io.joern.rubysrc2cpg.datastructures.BlockScope
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.passes.Defines.getBuiltInType
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.{NewBlock, NewFieldIdentifier, NewLiteral, NewLocal}
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, Operators}

trait AstForControlStructuresCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForControlStructureExpression(node: ControlFlowStatement): Ast = node match {
    case node: WhileExpression    => astForWhileStatement(node)
    case node: DoWhileExpression  => astForDoWhileStatement(node)
    case node: UntilExpression    => astForUntilStatement(node)
    case node: CaseExpression     => blockAst(NewBlock(), astsForCaseExpression(node).toList)
    case node: IfExpression       => astForIfExpression(node)
    case node: UnlessExpression   => astForUnlessStatement(node)
    case node: ForExpression      => astForForExpression(node)
    case node: RescueExpression   => astForRescueExpression(node)
    case node: NextExpression     => astForNextExpression(node)
    case node: BreakExpression    => astForBreakExpression(node)
    case node: OperatorAssignment => astForOperatorAssignmentExpression(node)
  }

  private def astForWhileStatement(node: WhileExpression): Ast = {
    val conditionAst = astForExpression(node.condition)
    val bodyAsts     = astsForStatement(node.body)
    whileAst(Some(conditionAst), bodyAsts, Option(code(node)), line(node), column(node))
  }

  private def astForDoWhileStatement(node: DoWhileExpression): Ast = {
    val conditionAst = astForExpression(node.condition)
    val bodyAsts     = astsForStatement(node.body)
    doWhileAst(Some(conditionAst), bodyAsts, Option(code(node)), line(node), column(node))
  }

  // `until T do B` is lowered as `while !T do B`
  private def astForUntilStatement(node: UntilExpression): Ast = {
    val notCondition = astForExpression(UnaryExpression("!", node.condition)(node.condition.span))
    val bodyAsts     = astsForStatement(node.body)
    whileAst(Some(notCondition), bodyAsts, Option(code(node)), line(node), column(node))
  }

  // Recursively lowers into a ternary conditional call
  private def astForIfExpression(node: IfExpression): Ast = {
    def builder(node: IfExpression, conditionAst: Ast, thenAst: Ast, elseAsts: List[Ast]): Ast = {
      // We want to make sure there's always an «else» clause in a ternary operator.
      // The default value is a `nil` literal.
      val elseAsts_ = if (elseAsts.isEmpty) {
        List(astForNilBlock)
      } else {
        elseAsts
      }

      val call = callNode(node, code(node), Operators.conditional, Operators.conditional, DispatchTypes.STATIC_DISPATCH)
      callAst(call, conditionAst :: thenAst :: elseAsts_)
    }

    // TODO: Remove or modify the builder pattern when we are no longer using ANTLR
    node.elseClause match {
      case Some(elseClause) =>
        elseClause match {
          case _: IfExpression => astForJsonIfStatement(node)
          case _               => foldIfExpression(builder)(node)
        }
      case None =>
        foldIfExpression(builder)(node)
    }
  }

  private def astForJsonIfStatement(node: IfExpression): Ast = {
    val conditionAst = astForExpression(node.condition)
    val thenAst      = astForThenClause(node.thenClause)
    val elseAsts = node.elseClause
      .map {
        case x: IfExpression =>
          val wrappedBlock = blockNode(x)
          Ast(wrappedBlock).withChild(astForJsonIfStatement(x))
        case x =>
          astForElseClause(x)
      }
      .getOrElse(Ast())

    val ifNode = controlStructureNode(node, ControlStructureTypes.IF, code(node))
    controlStructureAst(ifNode, Some(conditionAst), thenAst :: elseAsts :: Nil)
  }

  // `unless T do B` is lowered as `if !T then B`
  private def astForUnlessStatement(node: UnlessExpression): Ast = {
    val notConditionAst = astForExpression(UnaryExpression("!", node.condition)(node.condition.span))
    val thenAst = node.trueBranch match
      case stmtList: StatementList => astForStatementList(stmtList)
      case _                       => astForStatementList(StatementList(List(node.trueBranch))(node.trueBranch.span))
    val elseAsts = node.falseBranch.map(astForElseClause).toList
    val ifNode   = controlStructureNode(node, ControlStructureTypes.IF, code(node))
    controlStructureAst(ifNode, Some(notConditionAst), thenAst :: elseAsts)
  }

  protected def astForElseClause(node: RubyExpression): Ast = {
    node match
      case elseNode: ElseClause =>
        elseNode.thenClause match
          case stmtList: StatementList => astForStatementList(stmtList)
          case node =>
            logger.warn(s"Expecting statement list in ${code(node)} ($relativeFileName), skipping")
            astForUnknown(node)
      case elseNode =>
        logger.warn(s"Expecting else clause in ${code(elseNode)} ($relativeFileName), skipping")
        astForUnknown(elseNode)
  }

  private def astForForExpression(node: ForExpression): Ast = {
    val forEachNode = controlStructureNode(node, ControlStructureTypes.FOR, code(node))

    def collectionAst  = astForExpression(node.iterableVariable)
    val collectionNode = node.iterableVariable

    val iterIdentifier =
      identifierNode(
        node = node.forVariable,
        name = node.forVariable.span.text,
        code = node.forVariable.span.text,
        typeFullName = Defines.Any
      )
    val iterVarLocal = NewLocal().name(node.forVariable.span.text).code(node.forVariable.span.text)
    scope.addToScope(node.forVariable.span.text, iterVarLocal)

    val idxName  = "_idx_"
    val idxLocal = NewLocal().name(idxName).code(idxName).typeFullName(Defines.getBuiltInType(Defines.Integer))
    val idxIdenAtAssign = identifierNode(
      node = collectionNode,
      name = idxName,
      code = idxName,
      typeFullName = Defines.getBuiltInType(Defines.Integer)
    )

    val idxAssignment =
      callNode(node, s"$idxName = 0", Operators.assignment, Operators.assignment, DispatchTypes.STATIC_DISPATCH)
    val idxAssignmentArgs =
      List(Ast(idxIdenAtAssign), Ast(NewLiteral().code("0").typeFullName(Defines.getBuiltInType(Defines.Integer))))
    val idxAssignmentAst = callAst(idxAssignment, idxAssignmentArgs)

    val idxIdAtCond = idxIdenAtAssign.copy
    val collectionCountAccess = callNode(
      node,
      s"${node.iterableVariable.span.text}.length",
      Operators.fieldAccess,
      Operators.fieldAccess,
      DispatchTypes.STATIC_DISPATCH
    )
    val fieldAccessAst = callAst(
      collectionCountAccess,
      collectionAst :: Ast(NewFieldIdentifier().canonicalName("length").code("length")) :: Nil
    )

    val idxLt = callNode(
      node,
      s"$idxName < ${node.iterableVariable.span.text}.length",
      Operators.lessThan,
      Operators.lessThan,
      DispatchTypes.STATIC_DISPATCH
    )
    val idxLtArgs  = List(Ast(idxIdAtCond), fieldAccessAst)
    val ltCallCond = callAst(idxLt, idxLtArgs)

    val idxIdAtCollAccess = idxIdenAtAssign.copy
    val collectionIdxAccess = callNode(
      node,
      s"${node.iterableVariable.span.text}[$idxName++]",
      Operators.indexAccess,
      Operators.indexAccess,
      DispatchTypes.STATIC_DISPATCH
    )
    val postIncrAst = callAst(
      callNode(node, s"$idxName++", Operators.postIncrement, Operators.postIncrement, DispatchTypes.STATIC_DISPATCH),
      Ast(idxIdAtCollAccess) :: Nil
    )

    val indexAccessAst = callAst(collectionIdxAccess, collectionAst :: postIncrAst :: Nil)
    val iteratorAssignmentNode = callNode(
      node,
      s"${node.forVariable.span.text} = ${node.iterableVariable.span.text}[$idxName++]",
      Operators.assignment,
      Operators.assignment,
      DispatchTypes.STATIC_DISPATCH
    )
    val iteratorAssignmentArgs = List(Ast(iterIdentifier), indexAccessAst)
    val iteratorAssignmentAst  = callAst(iteratorAssignmentNode, iteratorAssignmentArgs)
    val doBodyAst              = astsForStatement(node.doBlock)

    val locals = Ast(idxLocal)
      .withRefEdge(idxIdenAtAssign, idxLocal)
      .withRefEdge(idxIdAtCond, idxLocal)
      .withRefEdge(idxIdAtCollAccess, idxLocal) :: Ast(iterVarLocal).withRefEdge(iterIdentifier, iterVarLocal) :: Nil

    val conditionAsts = ltCallCond :: Nil
    val initAsts      = idxAssignmentAst :: Nil
    val updateAsts    = iteratorAssignmentAst :: Nil

    forAst(
      forNode = forEachNode,
      locals = locals,
      initAsts = initAsts,
      conditionAsts = conditionAsts,
      updateAsts = updateAsts,
      bodyAsts = doBodyAst
    )
  }

  protected def astsForCaseExpression(node: CaseExpression): Seq[Ast] = {
    // TODO: Clean up the below
    def goCase(expr: Option[SimpleIdentifier]): List[RubyExpression] = {
      val elseThenClause: Option[RubyExpression] = node.elseClause.map(_.asInstanceOf[ElseClause].thenClause)
      val whenClauses                            = node.matchClauses.collect { case x: WhenClause => x }
      val inClauses                              = node.matchClauses.collect { case x: InClause => x }

      val ifElseChain = if (whenClauses.nonEmpty) {
        whenClauses.foldRight[Option[RubyExpression]](elseThenClause) {
          (whenClause: WhenClause, restClause: Option[RubyExpression]) =>
            // We translate multiple match expressions into an or expression.
            //
            // A single match expression is compared using `.===` to the case target expression if it is present
            // otherwise it is treated as a conditional.
            //
            // There may be a splat as the last match expression,
            // `case y when *x then c end` or
            // `case when *x then c end`
            // which is translated to `x.include? y` and `x.any?` conditions respectively

            val conditions = whenClause.matchExpressions.map { mExpr =>
              expr.map(e => BinaryExpression(mExpr, "===", e)(mExpr.span)).getOrElse(mExpr)
            } ++ whenClause.matchSplatExpression.iterator.flatMap {
              case splat @ SplattingRubyNode(exprList) =>
                expr
                  .map { e =>
                    List(MemberCall(exprList, ".", "include?", List(e))(splat.span))
                  }
                  .getOrElse {
                    List(MemberCall(exprList, ".", "any?", List())(splat.span))
                  }
              case e =>
                logger.warn(s"Unrecognised RubyNode (${e.getClass}) in case match splat expression")
                List(Unknown()(e.span))
            }
            // There is always at least one match expression or a splat
            // will become an unknown in condition at the end
            val condition = conditions.init.foldRight(conditions.last) { (cond, condAcc) =>
              BinaryExpression(cond, "||", condAcc)(whenClause.span)
            }
            val conditional = IfExpression(
              condition,
              whenClause.thenClause.asStatementList,
              List(),
              restClause.map { els => ElseClause(els.asStatementList)(els.span) }
            )(node.span)
            Some(conditional)
        }
      } else {
        inClauses.foldRight[Option[RubyExpression]](elseThenClause) {
          (inClause: InClause, restClause: Option[RubyExpression]) =>
            val (condition, body) = inClause.pattern match {
              case x: ArrayPattern =>
                val condition = expr.map(e => BinaryExpression(x, "===", e)(x.span)).getOrElse(inClause.pattern)
                val body      = inClause.body

                val stmts = x.children.zipWithIndex.flatMap {
                  case (lhs: MatchVariable, idx) if expr.isDefined =>
                    val arrAccess = {
                      val code    = s"${expr.get.text}[$idx]"
                      val base    = expr.get.copy()(expr.get.span.spanStart(expr.get.text))
                      val indices = StaticLiteral(idx.toString)(expr.get.span.spanStart(idx.toString)) :: Nil
                      IndexAccess(base, indices)(lhs.span.spanStart(code))
                    }
                    val asgn = SingleAssignment(lhs, "=", arrAccess)(
                      inClause.span.spanStart(s"${lhs.span.text} = ${expr.get.text}[$idx]")
                    )
                    Option(asgn)
                  case _ => None
                } :+ body
                val conditionBody = StatementList(stmts)(body.span)

                (condition, conditionBody)
              case x =>
                (x, inClause.body)
            }

            val conditional = IfExpression(
              condition,
              body,
              List.empty,
              restClause.map { els => ElseClause(els.asStatementList)(els.span) }
            )(node.span)
            Some(conditional)
        }
      }
      ifElseChain.iterator.toList
    }

    val caseExpr = node.expression
      .map {
        case arrayLiteral: ArrayLiteral =>
          val tmp             = SimpleIdentifier(None)(arrayLiteral.span.spanStart(this.tmpGen.fresh))
          val arrayLiteralAst = DummyAst(astForArrayLiteral(arrayLiteral))(arrayLiteral.span)
          (tmp, arrayLiteralAst)
        case e =>
          val tmp = SimpleIdentifier(None)(e.span.spanStart(this.tmpGen.fresh))
          (tmp, e)
      }
      .map((tmp, e) => StatementList(List(SingleAssignment(tmp, "=", e)(e.span)) ++ goCase(Some(tmp)))(node.span))
      .getOrElse(StatementList(goCase(None))(node.span))

    astsForStatement(caseExpr)
  }

  private def astForOperatorAssignmentExpression(node: OperatorAssignment): Ast = {
    val loweredAssignment = lowerAssignmentOperator(node.lhs, node.rhs, node.op, node.span)
    astForControlStructureExpression(loweredAssignment)
  }

}
