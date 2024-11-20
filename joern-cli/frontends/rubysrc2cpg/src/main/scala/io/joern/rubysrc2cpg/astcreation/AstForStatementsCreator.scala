package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.{RubyStatement, *}
import io.joern.rubysrc2cpg.datastructures.BlockScope
import io.joern.rubysrc2cpg.parser.RubyJsonHelpers
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.passes.Defines.getBuiltInType
import io.joern.x2cpg.datastructures.MethodLike
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.{NewBlock, NewControlStructure}
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, ModifierTypes, NodeTypes}

trait AstForStatementsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astsForStatement(node: RubyExpression): Seq[Ast] = {
    baseAstCache.clear() // A safe approximation on where to reset the cache
    node match {
      case node: IfExpression               => astForIfStatement(node)
      case node: OperatorAssignment         => astForOperatorAssignment(node)
      case node: CaseExpression             => astsForCaseExpression(node)
      case node: StatementList              => astForStatementList(node) :: Nil
      case node: ReturnExpression           => astForReturnExpression(node) :: Nil
      case node: AnonymousTypeDeclaration   => astForAnonymousTypeDeclaration(node) :: Nil
      case node: TypeDeclaration            => astForClassDeclaration(node)
      case node: FieldsDeclaration          => astsForFieldDeclarations(node)
      case node: AccessModifier             => astForAccessModifier(node)
      case node: MethodDeclaration          => astForMethodDeclaration(node)
      case node: MethodAccessModifier       => astForMethodAccessModifier(node)
      case node: SingletonMethodDeclaration => astForSingletonMethodDeclaration(node)
      case node: MultipleAssignment         => node.assignments.map(astForExpression)
      case node: BreakExpression            => astForBreakExpression(node) :: Nil
      case node: SingletonStatementList     => astForSingletonStatementList(node)
      case _                                => astForExpression(node) :: Nil
    }
  }

  private def astForIfStatement(node: IfExpression): Seq[Ast] = {
    def builder(node: IfExpression, conditionAst: Ast, thenAst: Ast, elseAsts: List[Ast]): Ast = {
      val ifNode = controlStructureNode(node, ControlStructureTypes.IF, code(node))
      controlStructureAst(ifNode, Some(conditionAst), thenAst :: elseAsts)
    }

    // TODO: Remove or modify the builder pattern when we are no longer using ANTLR
    node.elseClause match {
      case Some(elseClause) =>
        elseClause match {
          case _: IfExpression => astForJsonIfStatement(node)
          case _               => foldIfExpression(builder)(node) :: Nil
        }
      case None =>
        foldIfExpression(builder)(node) :: Nil
    }
  }

  private def astForOperatorAssignment(node: OperatorAssignment): Seq[Ast] = {
    val loweredAssignment = lowerAssignmentOperator(node.lhs, node.rhs, node.op, node.span)
    astsForStatement(loweredAssignment)
  }

  private def astForJsonIfStatement(node: IfExpression): Seq[Ast] = {
    val conditionAst = astForExpression(node.condition)
    val thenAst      = astForThenClause(node.thenClause)
    val elseAsts = node.elseClause
      .map {
        case x: IfExpression =>
          val wrappedBlock = blockNode(x)
          Ast(wrappedBlock).withChildren(astForJsonIfStatement(x)) :: Nil
        case x =>
          astForElseClause(x) :: Nil
      }
      .getOrElse(Ast() :: Nil)

    val ifNode = controlStructureNode(node, ControlStructureTypes.IF, code(node))
    controlStructureAst(ifNode, Some(conditionAst), thenAst +: elseAsts) :: Nil
  }

  private def astForAccessModifier(node: AccessModifier): Seq[Ast] = {
    scope.surroundingAstLabel match {
      case Some(x) if x == NodeTypes.METHOD =>
        val simpleIdent = node.toSimpleIdentifier
        astForSimpleCall(SimpleCall(simpleIdent, List.empty)(simpleIdent.span)) :: Nil
      case _ =>
        registerAccessModifier(node)
    }
  }

  /** Registers the currently set access modifier for the current type (until it is reset later).
    */
  private def registerAccessModifier(node: AccessModifier): Seq[Ast] = {
    val modifier = node match {
      case PrivateModifier()   => ModifierTypes.PRIVATE
      case ProtectedModifier() => ModifierTypes.PROTECTED
      case PublicModifier()    => ModifierTypes.PUBLIC
    }
    popAccessModifier()          // pop off the current modifier in scope
    pushAccessModifier(modifier) // push new one on
    Nil
  }

  // Rewrites a nested `if T_1 then E_1 elsif T_2 then E_2 elsif ... elsif T_n then E_n else E_{n+1}`
  // as `B(T_1, E_1, B(T_2, E_2, ..., B(T_n, E_n, E_{n+1})..)`
  protected def foldIfExpression(builder: (IfExpression, Ast, Ast, List[Ast]) => Ast)(node: IfExpression): Ast = {
    val conditionAst = astForExpression(node.condition)
    val thenAst      = astForThenClause(node.thenClause)
    val elseAsts     = astsForElseClauses(node.elsifClauses, node.elseClause, foldIfExpression(builder))
    builder(node, conditionAst, thenAst, elseAsts)
  }

  protected def astForThenClause(node: RubyExpression): Ast = astForStatementList(node.asStatementList)

  private def astsForElseClauses(
    elsIfClauses: List[RubyExpression],
    elseClause: Option[RubyExpression],
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

  protected def astForStatementList(node: StatementList): Ast = {
    val block = blockNode(node)
    scope.pushNewScope(BlockScope(block))
    val statementAsts = node.statements.flatMap(astsForStatement)
    scope.popScope()
    blockAst(block, statementAsts)
  }

  protected def astForDoBlock(block: Block & RubyExpression): Seq[Ast] = {
    if (closureToRefs.contains(block)) {
      closureToRefs(block).map(x => Ast(x.copy))
    } else {
      val methodName = nextClosureName()
      // Create closure structures: [TypeRef, MethodRef]
      val methodRefAsts = block.body match {
        case x: Block =>
          astForMethodDeclaration(x.toMethodDeclaration(methodName, Option(block.parameters)), isClosure = true)
        case _ =>
          astForMethodDeclaration(block.toMethodDeclaration(methodName, Option(block.parameters)), isClosure = true)
      }
      closureToRefs.put(block, methodRefAsts.flatMap(_.root))
      methodRefAsts
    }
  }

  protected def astForReturnExpression(node: ReturnExpression): Ast = {
    val argumentAsts = node.expressions.map(astForExpression)
    val returnNode_  = returnNode(node, code(node))
    returnAst(returnNode_, argumentAsts)
  }

  protected def astForNextExpression(node: NextExpression): Ast = {
    val nextNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.CONTINUE)
      .lineNumber(line(node))
      .columnNumber(column(node))
      .code(code(node))
    Ast(nextNode)
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

  private def astsForImplicitReturnStatement(node: RubyExpression): Seq[Ast] = {
    def elseReturnNil(span: TextSpan) = Option {
      ElseClause(
        StatementList(
          ReturnExpression(StaticLiteral(getBuiltInType(Defines.NilClass))(span.spanStart("nil")) :: Nil)(
            span.spanStart("return nil")
          ) :: Nil
        )(span.spanStart("return nil"))
      )(span.spanStart("else\n\treturn nil\nend"))
    }

    node match
      case expr: ControlFlowStatement =>
        def transform(e: RubyExpression & ControlFlowStatement): RubyExpression =
          transformLastRubyNodeInControlFlowExpressionBody(e, returnLastNode(_, transform), elseReturnNil)

        expr match {
          case x @ OperatorAssignment(lhs, op, rhs) =>
            val loweredAssignment = lowerAssignmentOperator(lhs, rhs, op, x.span)
            astsForStatement(transform(loweredAssignment))
          case x =>
            astsForStatement(transform(expr))
        }
      case node: MemberCallWithBlock => returnAstForRubyCall(node)
      case node: SimpleCallWithBlock => returnAstForRubyCall(node)
      case _: (LiteralExpr | BinaryExpression | UnaryExpression | SimpleIdentifier | SelfIdentifier | IndexAccess |
            Association | YieldExpr | RubyCall | RubyFieldIdentifier | HereDocNode | Unknown) =>
        astForReturnExpression(ReturnExpression(List(node))(node.span)) :: Nil
      case node: SingleAssignment =>
        astForSingleAssignment(node) :: List(astForReturnExpression(ReturnExpression(List(node.lhs))(node.span)))
      case node: DefaultMultipleAssignment =>
        astsForStatement(node) ++ astsForImplicitReturnStatement(ArrayLiteral(node.assignments.map(_.lhs))(node.span))
      case node: GroupedParameterDesugaring =>
        // If the desugaring is the last expression, then we should return nil
        val nilReturnSpan    = node.span.spanStart("return nil")
        val nilReturnLiteral = StaticLiteral(Defines.NilClass)(nilReturnSpan)
        astsForStatement(node) ++ astsForImplicitReturnStatement(nilReturnLiteral)
      case node: AttributeAssignment =>
        List(
          astForAttributeAssignment(node),
          astForReturnFieldAccess(MemberAccess(node.target, node.op, node.attributeName)(node.span))
        )
      case node: MemberAccess    => astForReturnMemberCall(node) :: Nil
      case ret: ReturnExpression => astForReturnExpression(ret) :: Nil
      case node: (MethodDeclaration | SingletonMethodDeclaration) =>
        (astsForStatement(node) :+ astForReturnMethodDeclarationSymbolName(node)).toList
      case stmtList: StatementList if stmtList.statements.lastOption.exists(_.isInstanceOf[ReturnExpression]) =>
        stmtList.statements.map(astForExpression)
      case StatementList(stmts) =>
        val nilReturnSpan    = node.span.spanStart("return nil")
        val nilReturnLiteral = StaticLiteral(Defines.NilClass)(nilReturnSpan)
        stmts.map(astForExpression) ++ astsForImplicitReturnStatement(nilReturnLiteral)
      case x: RangeExpression =>
        astForReturnRangeExpression(x) :: Nil
      case node: AccessModifier =>
        val simpleIdent = node.toSimpleIdentifier
        val simpleCall  = SimpleCall(simpleIdent, List.empty)(simpleIdent.span)
        astForReturnExpression(ReturnExpression(List(simpleCall))(node.span)) :: Nil
      case node =>
        logger.warn(s" not supported yet: ${node.text} (${node.getClass.getSimpleName}), only generating statement")
        astsForStatement(node).toList
  }

  private def returnAstForRubyCall[C <: RubyCall](node: RubyExpression & RubyCallWithBlock[C]): Seq[Ast] = {
    val callAst = astForCallWithBlock(node)
    returnAst(returnNode(node, code(node)), List(callAst)) :: Nil
  }

  private def astForReturnFieldAccess(node: MemberAccess): Ast = {
    returnAst(returnNode(node, code(node)), List(astForFieldAccess(node)))
  }

  // The evaluation of a MethodDeclaration returns its name in symbol form.
  // E.g. `def f = 0` ===> `:f`
  private def astForReturnMethodDeclarationSymbolName(node: RubyExpression & ProcedureDeclaration): Ast = {
    val literalNode_ = literalNode(node, s":${node.methodName}", getBuiltInType(Defines.Symbol))
    val returnNode_  = returnNode(node, literalNode_.code)
    returnAst(returnNode_, Seq(Ast(literalNode_)))
  }

  private def astForReturnRangeExpression(node: RangeExpression): Ast = {
    returnAst(returnNode(node, code(node)), List(astForRange(node)))
  }

  private def astForReturnMemberCall(node: MemberAccess): Ast = {
    returnAst(returnNode(node, code(node)), List(astForMemberAccess(node)))
  }

  protected def astForBreakExpression(node: BreakExpression): Ast = {
    val _node = NewControlStructure()
      .controlStructureType(ControlStructureTypes.BREAK)
      .lineNumber(line(node))
      .columnNumber(column(node))
      .code(code(node))
    Ast(_node)
  }

  protected def astForSingletonStatementList(list: SingletonStatementList): Seq[Ast] = {
    list.statements.map(astForExpression)
  }

  /** Wraps the last RubyNode with a ReturnExpression.
    * @param x
    *   the node to wrap a return around. If a StatementList is given, then the ReturnExpression will wrap around the
    *   final element.
    * @return
    *   the RubyNode with an explicit expression
    */
  private def returnLastNode(
    x: RubyExpression,
    transform: (RubyExpression & ControlFlowStatement) => RubyExpression
  ): RubyExpression = {
    def statementListReturningLastExpression(stmts: List[RubyExpression]): List[RubyExpression] = stmts match {
      case (head: ControlFlowClause) :: Nil    => clauseReturningLastExpression(head) :: Nil
      case (head: ControlFlowStatement) :: Nil => transform(head) :: Nil
      case (head: ReturnExpression) :: Nil     => head :: Nil
      case head :: Nil                         => ReturnExpression(head :: Nil)(head.span) :: Nil
      case Nil                                 => List.empty
      case head :: tail                        => head :: statementListReturningLastExpression(tail)
    }

    def clauseReturningLastExpression(x: RubyExpression & ControlFlowClause): RubyExpression = x match {
      case RescueClause(exceptionClassList, assignment, thenClause) =>
        RescueClause(exceptionClassList, assignment, returnLastNode(thenClause, transform))(x.span)
      case EnsureClause(thenClause)           => EnsureClause(returnLastNode(thenClause, transform))(x.span)
      case ElsIfClause(condition, thenClause) => ElsIfClause(condition, returnLastNode(thenClause, transform))(x.span)
      case ElseClause(thenClause)             => ElseClause(returnLastNode(thenClause, transform))(x.span)
      case WhenClause(matchExpressions, matchSplatExpression, thenClause) =>
        WhenClause(matchExpressions, matchSplatExpression, returnLastNode(thenClause, transform))(x.span)
    }

    x match {
      case StatementList(statements)  => StatementList(statementListReturningLastExpression(statements))(x.span)
      case clause: ControlFlowClause  => clauseReturningLastExpression(clause)
      case node: ControlFlowStatement => transform(node)
      case node: ReturnExpression     => node
      case _                          => ReturnExpression(x :: Nil)(x.span)
    }
  }

  /** @param node
    *   \- Control Flow Expression RubyNode
    * @param transform
    *   \- RubyNode => RubyNode function for transformation on the clauses of the ControlFlowExpression
    * @return
    *   RubyNode with transform function applied
    */
  protected def transformLastRubyNodeInControlFlowExpressionBody(
    node: RubyExpression & ControlFlowStatement,
    transform: RubyExpression => RubyExpression,
    defaultElseBranch: TextSpan => Option[ElseClause]
  ): RubyExpression = {
    node match {
      case RescueExpression(body, rescueClauses, elseClause, ensureClause) =>
        // Ensure never returns a value, only the main body, rescue & else clauses
        RescueExpression(
          transform(body),
          rescueClauses.map(transform).collect { case x: RescueClause => x },
          elseClause.map(transform).orElse(defaultElseBranch(node.span)).collect { case x: ElseClause => x },
          ensureClause
        )(node.span)
      case WhileExpression(condition, body)   => WhileExpression(condition, transform(body))(node.span)
      case DoWhileExpression(condition, body) => DoWhileExpression(condition, transform(body))(node.span)
      case UntilExpression(condition, body)   => UntilExpression(condition, transform(body))(node.span)
      case OperatorAssignment(lhs, op, rhs) =>
        val loweredNode = lowerAssignmentOperator(lhs, rhs, op, node.span)
        transformLastRubyNodeInControlFlowExpressionBody(loweredNode, transform, defaultElseBranch)
      case IfExpression(condition, thenClause, elsifClauses, elseClause) =>
        IfExpression(
          condition,
          transform(thenClause),
          elsifClauses.map(transform),
          elseClause.map(transform).orElse(defaultElseBranch(node.span))
        )(node.span)
      case UnlessExpression(condition, trueBranch, falseBranch) =>
        UnlessExpression(
          condition,
          transform(trueBranch),
          falseBranch.map(transform).orElse(defaultElseBranch(node.span))
        )(node.span)
      case ForExpression(forVariable, iterableVariable, doBlock) =>
        ForExpression(forVariable, iterableVariable, transform(doBlock))(node.span)
      case CaseExpression(expression, whenClauses, elseClause) =>
        CaseExpression(
          expression,
          whenClauses.map(transform),
          elseClause.map(transform).orElse(defaultElseBranch(node.span))
        )(node.span)
      case next: NextExpression   => next
      case break: BreakExpression => break
    }
  }
}
