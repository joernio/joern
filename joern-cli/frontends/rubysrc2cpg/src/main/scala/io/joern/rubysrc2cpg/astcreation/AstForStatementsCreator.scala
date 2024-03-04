package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.*
import io.joern.rubysrc2cpg.datastructures.BlockScope
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.passes.Defines.getBuiltInType
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.nodes.{NewMethod, NewMethodRef, NewTypeDecl}

trait AstForStatementsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astsForStatement(node: RubyNode): Seq[Ast] = node match
    case node: WhileExpression            => astForWhileStatement(node) :: Nil
    case node: UntilExpression            => astForUntilStatement(node) :: Nil
    case node: IfExpression               => astForIfStatement(node) :: Nil
    case node: UnlessExpression           => astForUnlessStatement(node) :: Nil
    case node: ForExpression              => astForForExpression(node) :: Nil
    case node: CaseExpression             => astsForCaseExpression(node)
    case node: StatementList              => astForStatementList(node) :: Nil
    case node: SimpleCallWithBlock        => astsForCallWithBlock(node)
    case node: MemberCallWithBlock        => astsForCallWithBlock(node)
    case node: ReturnExpression           => astForReturnStatement(node) :: Nil
    case node: AnonymousTypeDeclaration   => astForAnonymousTypeDeclaration(node) :: Nil
    case node: TypeDeclaration            => astForClassDeclaration(node) :: Nil
    case node: FieldsDeclaration          => astsForFieldDeclarations(node)
    case node: MethodDeclaration          => astForMethodDeclaration(node)
    case node: SingletonMethodDeclaration => astForSingletonMethodDeclaration(node) :: Nil
    case node: MultipleAssignment         => node.assignments.map(astForExpression)
    case _                                => astForExpression(node) :: Nil

  private def astForWhileStatement(node: WhileExpression): Ast = {
    val conditionAst = astForExpression(node.condition)
    val bodyAsts     = astsForStatement(node.body)
    whileAst(Some(conditionAst), bodyAsts, Option(code(node)), line(node), column(node))
  }

  // `until T do B` is lowered as `while !T do B`
  private def astForUntilStatement(node: UntilExpression): Ast = {
    val notCondition = astForExpression(UnaryExpression("!", node.condition)(node.condition.span))
    val bodyAsts     = astsForStatement(node.body)
    whileAst(Some(notCondition), bodyAsts, Option(code(node)), line(node), column(node))
  }

  private def astForIfStatement(node: IfExpression): Ast = {
    def builder(node: IfExpression, conditionAst: Ast, thenAst: Ast, elseAsts: List[Ast]): Ast = {
      val ifNode = controlStructureNode(node, ControlStructureTypes.IF, code(node))
      controlStructureAst(ifNode, Some(conditionAst), thenAst :: elseAsts)
    }
    foldIfExpression(builder)(node)
  }

  // Rewrites a nested `if T_1 then E_1 elsif T_2 then E_2 elsif ... elsif T_n then E_n else E_{n+1}`
  // as `B(T_1, E_1, B(T_2, E_2, ..., B(T_n, E_n, E_{n+1})..)`
  protected def foldIfExpression(builder: (IfExpression, Ast, Ast, List[Ast]) => Ast)(node: IfExpression): Ast = {
    val conditionAst = astForExpression(node.condition)
    val thenAst      = astForThenClause(node.thenClause)
    val elseAsts     = astsForElseClauses(node.elsifClauses, node.elseClause, foldIfExpression(builder))
    builder(node, conditionAst, thenAst, elseAsts)
  }

  private def astForThenClause(node: RubyNode): Ast = astForStatementList(node.asStatementList)

  private def astsForElseClauses(
    elsIfClauses: List[RubyNode],
    elseClause: Option[RubyNode],
    astForIf: IfExpression => Ast
  ): List[Ast] = {
    elsIfClauses match
      case Nil => elseClause.map(astForElseClause).toList
      case elsIfNode :: rest =>
        elsIfNode match
          case elsIfNode: ElsIfClause =>
            val newIf = IfExpression(elsIfNode.condition, elsIfNode.thenClause, rest, elseClause)(elsIfNode.span)
            val wrappingBlock = blockNode(elsIfNode)
            val wrappedAst    = Ast(wrappingBlock).withChild(astForIf(newIf))
            wrappedAst :: Nil
          case elsIfNode =>
            logger.warn(s"Expecting elsif clause in ${code(elsIfNode)} ($relativeFileName), skipping")
            Nil
  }

  private def astForElseClause(node: RubyNode): Ast = {
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

  private def astForForExpression(node: ForExpression): Ast = {
    val forEachNode  = controlStructureNode(node, ControlStructureTypes.FOR, code(node))
    val doBodyAst    = astsForStatement(node.doBlock)
    val iteratorNode = astForExpression(node.forVariable)
    val iterableNode = astForExpression(node.iterableVariable)
    Ast(forEachNode).withChild(iteratorNode).withChild(iterableNode).withChildren(doBodyAst)
  }

  protected def astsForCaseExpression(node: CaseExpression): Seq[Ast] = {
    def goCase(expr: Option[SimpleIdentifier]): List[RubyNode] = {
      val elseThenClause: Option[RubyNode] = node.elseClause.map(_.asInstanceOf[ElseClause].thenClause)
      val whenClauses                      = node.whenClauses.map(_.asInstanceOf[WhenClause])
      val ifElseChain = whenClauses.foldRight[Option[RubyNode]](elseThenClause) {
        (whenClause: WhenClause, restClause: Option[RubyNode]) =>
          // We translate multiple match expressions into an or expression.
          // There may be a splat as the last match expression, which is currently parsed as unknown
          // A single match expression is compared using `.===` to the case target expression if it is present
          // otherwise it is treated as a conditional.
          val conditions = whenClause.matchExpressions.map { mExpr =>
            expr.map(e => MemberCall(mExpr, ".", "===", List(e))(mExpr.span)).getOrElse(mExpr)
          } ++ (whenClause.matchSplatExpression.iterator.flatMap {
            case u: Unknown => List(u)
            case e =>
              logger.warn("Splatting not implemented for `when` in ruby `case`")
              List(Unknown()(e.span))
          })
          // There is always at least one match expression or a splat
          // a splat will become an unknown in condition at the end
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
      ifElseChain.iterator.toList
    }
    def generatedNode: StatementList = node.expression
      .map { e =>
        val tmp = SimpleIdentifier(None)(e.span.spanStart(freshVariableName))
        StatementList(
          List(SingleAssignment(tmp, "=", e)(e.span)) ++
            goCase(Some(tmp))
        )(node.span)
      }
      .getOrElse(StatementList(goCase(None))(node.span))
    astsForStatement(generatedNode)
  }

  protected def astForStatementList(node: StatementList): Ast = {
    val block = blockNode(node)
    scope.pushNewScope(BlockScope(block))
    val statementAsts = node.statements.flatMap(astsForStatement)
    scope.popScope()
    blockAst(block, statementAsts)
  }

  /* `foo(<args>) do <params> <stmts> end` is lowered as a METHOD node shaped like so:
   * ```
   * <method_ref> = def <lambda>0(<params>)
   *   <stmts>
   * end
   * foo(<args>, <method_ref>)
   * ```
   */
  private def astsForCallWithBlock[C <: RubyCall](node: RubyNode with RubyCallWithBlock[C]): Seq[Ast] = {
    val Seq(methodDecl, typeDecl, _, methodRef) = astForDoBlock(node.block): @unchecked
    val methodRefDummyNode                      = methodRef.root.map(DummyNode(_)(node.span)).toList

    // Create call with argument referencing the MethodRef
    val callWithLambdaArg = node.withoutBlock match {
      case x: SimpleCall => astForSimpleCall(x.copy(arguments = x.arguments ++ methodRefDummyNode)(x.span))
      case x: MemberCall => astForMemberCall(x.copy(arguments = x.arguments ++ methodRefDummyNode)(x.span))
      case x =>
        logger.warn(s"Unhandled call-with-block type ${code(x)}, creating anonymous method structures only")
        Ast()
    }

    methodDecl :: typeDecl :: methodRef :: callWithLambdaArg :: Nil
  }

  protected def astForDoBlock(block: Block with RubyNode): Seq[Ast] = {
    // Create closure structures: [MethodDecl, TypeRef, MethodRef]
    val methodName         = nextClosureName()
    val methodAstsWithRefs = astForMethodDeclaration(block.toMethodDeclaration(methodName), isClosure = true)
    // Set span contents
    methodAstsWithRefs.flatMap(_.nodes).foreach {
      case m: NewMethodRef => DummyNode(m.copy)(block.span.spanStart(m.code))
      case _               =>
    }

    methodAstsWithRefs
  }

  protected def astForReturnStatement(node: ReturnExpression): Ast = {
    val argumentAsts = node.expressions.map(astForExpression)
    val returnNode_  = returnNode(node, code(node))
    returnAst(returnNode_, argumentAsts)
  }

  protected def astForStatementListReturningLastExpression(node: StatementList): Ast = {
    val block = blockNode(node)
    scope.pushNewScope(BlockScope(block))

    val stmtAsts = node.statements.size match
      case 0 => List()
      case n =>
        val (headStmts, lastStmt) = node.statements.splitAt(n - 1)
        headStmts.flatMap(astsForStatement) ++ lastStmt.flatMap(astsForImplicitReturnStatement)

    scope.popScope()
    blockAst(block, stmtAsts)
  }

  private def astsForImplicitReturnStatement(node: RubyNode): Seq[Ast] = {
    node match
      case expr: ControlFlowExpression => astsForStatement(rubyNodeForExplicitReturnControlFlowExpression(expr))
      case _: (LiteralExpr | BinaryExpression | UnaryExpression | SimpleIdentifier | SimpleCall | IndexAccess |
            Association) =>
        astForReturnStatement(ReturnExpression(List(node))(node.span)) :: Nil
      case node: SingleAssignment =>
        astForSingleAssignment(node) :: List(astForReturnStatement(ReturnExpression(List(node.lhs))(node.span)))
      case node: AttributeAssignment =>
        List(
          astForAttributeAssignment(node),
          astForReturnFieldAccess(MemberAccess(node.target, node.op, node.attributeName)(node.span))
        )
      case node: MemberAccess    => astForReturnMemberCall(node) :: Nil
      case node: MemberCall      => astForReturnMemberCall(node) :: Nil
      case ret: ReturnExpression => astForReturnStatement(ret) :: Nil
      case node: MethodDeclaration =>
        (astForMethodDeclaration(node) :+ astForReturnMethodDeclarationSymbolName(node)).toList
      case node =>
        logger.warn(
          s"Implicit return here not supported yet: ${node.text} (${node.getClass.getSimpleName}), only generating statement"
        )
        astsForStatement(node).toList
  }

  private def astForReturnFieldAccess(node: MemberAccess): Ast = {
    returnAst(returnNode(node, code(node)), List(astForFieldAccess(node)))
  }

  // The evaluation of a MethodDeclaration returns its name in symbol form.
  // E.g. `def f = 0` ===> `:f`
  private def astForReturnMethodDeclarationSymbolName(node: MethodDeclaration): Ast = {
    val literalNode_ = literalNode(node, s":${node.methodName}", getBuiltInType(Defines.Symbol))
    val returnNode_  = returnNode(node, literalNode_.code)
    returnAst(returnNode_, Seq(Ast(literalNode_)))
  }

  private def astForReturnMemberCall(node: MemberAccess): Ast = {
    returnAst(returnNode(node, code(node)), List(astForMemberAccess(node)))
  }

  private def astForReturnMemberCall(node: MemberCall): Ast = {
    returnAst(returnNode(node, code(node)), List(astForMemberCall(node)))
  }

  /** Wraps the last RubyNode with a ReturnExpression.
    * @param x
    *   the node to wrap a return around. If a StatementList is given, then the ReturnExpression will wrap around the
    *   final element.
    * @return
    *   the RubyNode with an explicit ReturnExpression.
    */
  private def returnLastNode(x: RubyNode): RubyNode = {

    def statementListReturningLastExpression(stmts: List[RubyNode]): List[RubyNode] = stmts match {
      case (head: ControlFlowClause) :: Nil => clauseReturningLastExpression(head) :: Nil
      case head :: Nil                      => ReturnExpression(head :: Nil)(head.span) :: Nil
      case Nil                              => List.empty
      case head :: tail                     => head :: statementListReturningLastExpression(tail)
    }

    def clauseReturningLastExpression(x: RubyNode with ControlFlowClause): RubyNode = x match {
      case RescueClause(exceptionClassList, assignment, thenClause) =>
        RescueClause(exceptionClassList, assignment, returnLastNode(thenClause))(x.span)
      case EnsureClause(thenClause)           => EnsureClause(returnLastNode(thenClause))(x.span)
      case ElsIfClause(condition, thenClause) => ElsIfClause(condition, returnLastNode(thenClause))(x.span)
      case ElseClause(thenClause)             => ElseClause(returnLastNode(thenClause))(x.span)
      case WhenClause(matchExpressions, matchSplatExpression, thenClause) =>
        WhenClause(matchExpressions, matchSplatExpression, returnLastNode(thenClause))(x.span)
    }

    x match {
      case StatementList(statements) => StatementList(statementListReturningLastExpression(statements))(x.span)
      case clause: ControlFlowClause => clauseReturningLastExpression(clause)
      case _                         => ReturnExpression(x :: Nil)(x.span)
    }
  }

  private def rubyNodeForExplicitReturnControlFlowExpression(node: RubyNode with ControlFlowExpression): RubyNode = {

    /** For missing else-branches, we want to make sure there is an implicit nil return
      */
    def elseReturnNil = Option {
      ElseClause(
        StatementList(
          ReturnExpression(StaticLiteral(getBuiltInType(Defines.NilClass))(node.span.spanStart("nil")) :: Nil)(
            node.span.spanStart("return nil")
          ) :: Nil
        )(node.span.spanStart("return nil"))
      )(node.span.spanStart("else\n\treturn nil\nend"))
    }

    node match {
      case RescueExpression(body, rescueClauses, elseClause, ensureClause) =>
        // Ensure never returns a value, only the main body, rescue & else clauses
        RescueExpression(
          returnLastNode(body),
          rescueClauses.map(returnLastNode),
          elseClause.map(returnLastNode).orElse(elseReturnNil),
          ensureClause
        )(node.span)
      case WhileExpression(condition, body) => WhileExpression(condition, returnLastNode(body))(node.span)
      case UntilExpression(condition, body) => UntilExpression(condition, returnLastNode(body))(node.span)
      case IfExpression(condition, thenClause, elsifClauses, elseClause) =>
        IfExpression(
          condition,
          returnLastNode(thenClause),
          elsifClauses.map(returnLastNode),
          elseClause.map(returnLastNode).orElse(elseReturnNil)
        )(node.span)
      case UnlessExpression(condition, trueBranch, falseBranch) =>
        UnlessExpression(condition, returnLastNode(trueBranch), falseBranch.map(returnLastNode).orElse(elseReturnNil))(
          node.span
        )
      case ForExpression(forVariable, iterableVariable, doBlock) =>
        ForExpression(forVariable, iterableVariable, returnLastNode(doBlock))(node.span)
      case CaseExpression(expression, whenClauses, elseClause) =>
        CaseExpression(
          expression,
          whenClauses.map(returnLastNode),
          elseClause.map(returnLastNode).orElse(elseReturnNil)
        )(node.span)
    }
  }
}
