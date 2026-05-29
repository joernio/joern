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
  ElseClause,
  ForExpression,
  IfExpression,
  InClause,
  IndexAccess,
  MatchVariable,
  MemberCall,
  NextExpression,
  OperatorAssignment,
  RegexMatchMemberCall,
  RescueExpression,
  RubyExpression,
  SimpleIdentifier,
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
import io.joern.rubysrc2cpg.passes.Defines.RubyOperators
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.{NewBlock, NewFieldIdentifier, NewLiteral, NewLocal}
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, Operators}

trait AstForControlStructuresCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForControlStructureExpression(node: ControlFlowStatement): Ast = node match {
    case node: WhileExpression    => astForWhileExpression(node)
    case node: DoWhileExpression  => astForDoWhileExpression(node)
    case node: UntilExpression    => astForUntilExpression(node)
    case node: CaseExpression     => astForCaseAsExpression(node)
    case node: IfExpression       => astForIfExpression(node)
    case node: UnlessExpression   => astForUnlessExpression(node)
    case node: ForExpression      => astForForExpression(node)
    case node: RescueExpression   => astForRescueExpression(node)
    case node: NextExpression     => astForNextExpression(node)
    case node: BreakExpression    => astForBreakExpression(node)
    case node: OperatorAssignment => astForOperatorAssignmentExpression(node)
  }

  private def astForWhileExpression(node: WhileExpression): Ast =
    loopExpressionAst(astForWhileStatement(node), node)

  private def astForDoWhileExpression(node: DoWhileExpression): Ast =
    loopExpressionAst(astForDoWhileStatement(node), node)

  private def astForUntilExpression(node: UntilExpression): Ast =
    loopExpressionAst(astForUntilStatement(node), node)

  private def loopExpressionAst(loopAst: Ast, node: RubyExpression): Ast = {
    val nilSpan = node.span.spanStart("nil")
    val nilLit = literalNode(
      StaticLiteral(Defines.prefixAsCoreType(Defines.NilClass))(nilSpan),
      "nil",
      Defines.prefixAsCoreType(Defines.NilClass)
    )
    blockAst(blockNode(node), List(loopAst, Ast(nilLit)))
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

  private def wrapNestedIf(nested: IfExpression): Ast =
    Ast(blockNode(nested)).withChild(astForIfExpression(nested))

  private def astForIfExpression(node: IfExpression): Ast = {
    val conditionAst = astForExpression(node.condition)
    val thenAst      = astForThenClause(node.thenClause)
    val elseAst = node.elseClause match {
      case Some(nested: IfExpression)                                  => wrapNestedIf(nested)
      case Some(ElseClause(StatementList(List(nested: IfExpression)))) => wrapNestedIf(nested)
      case Some(other)                                                 => astForElseClause(other)
      case None                                                        => astForNilBlock
    }

    val call = callNode(node, code(node), Operators.conditional, Operators.conditional, DispatchTypes.STATIC_DISPATCH)
    callAst(call, conditionAst :: thenAst :: elseAst :: Nil)
  }

  protected def astForUnlessExpression(node: UnlessExpression): Ast = {
    val conditionAst = astForExpression(node.condition)
    val notCall = callNode(
      node.condition,
      s"!${code(node.condition)}",
      Operators.logicalNot,
      Operators.logicalNot,
      DispatchTypes.STATIC_DISPATCH
    )
    val notConditionAst = callAst(notCall, List(conditionAst))
    val thenAst         = astForThenClause(node.trueBranch)
    val elseAst = node.falseBranch match {
      case Some(nested: IfExpression)                                  => wrapNestedIf(nested)
      case Some(ElseClause(StatementList(List(nested: IfExpression)))) => wrapNestedIf(nested)
      case Some(other)                                                 => astForElseClause(other)
      case None                                                        => astForNilBlock
    }

    val call = callNode(node, code(node), Operators.conditional, Operators.conditional, DispatchTypes.STATIC_DISPATCH)
    callAst(call, notConditionAst :: thenAst :: elseAst :: Nil)
  }

  protected def astForElseClause(node: RubyExpression): Ast = {
    node match {
      case elseNode: ElseClause =>
        elseNode.thenClause match {
          case stmtList: StatementList => astForStatementList(stmtList)
          case node =>
            logger.warn(s"Expecting statement list in ${code(node)} ($relativeFileName), skipping")
            astForUnknown(node)
        }
      case elseNode =>
        logger.warn(s"Expecting else clause in ${code(elseNode)} ($relativeFileName), skipping")
        astForUnknown(elseNode)
    }
  }

  private def astForForExpression(node: ForExpression): Ast = {
    blockAst(blockNode(node), List(astForForStatement(node), astForExpression(node.iterableVariable)))
  }

  private def astForForStatement(node: ForExpression): Ast = {
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
    val idxLocal = NewLocal().name(idxName).code(idxName).typeFullName(Defines.prefixAsCoreType(Defines.Integer))
    val idxIdenAtAssign = identifierNode(
      node = collectionNode,
      name = idxName,
      code = idxName,
      typeFullName = Defines.prefixAsCoreType(Defines.Integer)
    )

    val idxAssignment =
      callNode(node, s"$idxName = 0", Operators.assignment, Operators.assignment, DispatchTypes.STATIC_DISPATCH)
    val idxAssignmentArgs =
      List(Ast(idxIdenAtAssign), Ast(NewLiteral().code("0").typeFullName(Defines.prefixAsCoreType(Defines.Integer))))
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

  protected def lowerCaseToStatementList(node: CaseExpression): StatementList = {
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

            val conditions = whenClause.matchExpressions.map {
              case regex: StaticLiteral if regex.typeFullName == prefixAsCoreType(Defines.Regexp) =>
                expr
                  .map(e => RegexMatchMemberCall(regex, ".", RubyOperators.regexpMatch, e :: Nil)(regex.span))
                  .getOrElse(regex)
              case mExpr =>
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
                      val code_ = s"${code(expr.get)}[$idx]"
                      val base  = expr.get.copy()(expr.get.span.spanStart(code(expr.get)))
                      val indices = StaticLiteral(Defines.prefixAsCoreType(Defines.Integer))(
                        expr.get.span.spanStart(idx.toString)
                      ) :: Nil
                      IndexAccess(base, indices)(lhs.span.spanStart(code_))
                    }
                    val asgn = SingleAssignment(lhs, "=", arrAccess)(
                      inClause.span.spanStart(s"${lhs.span.text} = ${code(expr.get)}[$idx]")
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

    node.expression
      .map {
        case arrayLiteral: ArrayLiteral =>
          val tmp             = SimpleIdentifier(None)(arrayLiteral.span.spanStart(scope.getNewVarTmp))
          val arrayLiteralAst = DummyAst(astForArrayLiteral(arrayLiteral))(arrayLiteral.span)
          (tmp, arrayLiteralAst)
        case e =>
          val tmp = SimpleIdentifier(None)(e.span.spanStart(scope.getNewVarTmp))
          (tmp, e)
      }
      .map((tmp, e) => StatementList(List(SingleAssignment(tmp, "=", e)(e.span)) ++ goCase(Some(tmp)))(node.span))
      .getOrElse(StatementList(goCase(None))(node.span))
  }

  protected def astsForCaseExpression(node: CaseExpression): Seq[Ast] = {
    astsForStatement(lowerCaseToStatementList(node))
  }

  protected def astForCaseAsExpression(node: CaseExpression): Ast = {
    val lowered = lowerCaseToStatementList(node)
    val stmts   = lowered.statements
    val block   = blockNode(node)
    scope.pushNewScope(BlockScope(block))
    val (init, last) = stmts.splitAt(stmts.length - 1)
    val initAsts     = init.flatMap(astsForStatement)
    val exprAst = last.headOption match {
      case Some(other) => astForExpression(other)
      case None        => astForNilBlock
    }
    val result = blockAst(block, initAsts :+ exprAst)
    scope.popScope()
    result
  }

  private def astForOperatorAssignmentExpression(node: OperatorAssignment): Ast = {
    val loweredAssignment = lowerAssignmentOperator(node.lhs, node.rhs, node.op, node.span)
    astForControlStructureExpression(loweredAssignment)
  }

}
