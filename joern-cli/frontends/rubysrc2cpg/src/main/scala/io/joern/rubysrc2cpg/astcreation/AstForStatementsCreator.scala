package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.*
import io.joern.rubysrc2cpg.datastructures.BlockScope
import io.joern.rubysrc2cpg.parser.RubyJsonHelpers
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.passes.Defines.prefixAsKernelDefined
import io.joern.x2cpg.datastructures.MethodLike
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.{NewBlock, NewControlStructure}
import io.shiftleft.codepropertygraph.generated.{
  ControlStructureTypes,
  DispatchTypes,
  ModifierTypes,
  NodeTypes,
  Operators
}

import scala.collection.mutable

trait AstForStatementsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astsForStatement(node: RubyExpression): Seq[Ast] = {
    baseAstCache.clear() // A safe approximation on where to reset the cache
    node match {
      case node: IfExpression               => astForIfStatement(conditionalStatementBuilder)(node) :: Nil
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
      case node: DefaultMultipleAssignment  => astForDefaultMultipleAssignment(node, isExpression = false)
      case node: MultipleAssignment         => node.assignments.map(astForExpression)
      case node: BreakExpression            => astForBreakExpression(node) :: Nil
      case node: SingletonStatementList     => astForSingletonStatementList(node)
      case node: AliasStatement             => astForAliasStatement(node)
      case _                                => astForExpression(node) :: Nil
    }
  }

  private def astForOperatorAssignment(node: OperatorAssignment): Seq[Ast] = {
    val loweredAssignment = lowerAssignmentOperator(node.lhs, node.rhs, node.op, node.span)
    astsForStatement(loweredAssignment)
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
  protected def foldIfExpression(builder: (IfExpression, Ast, Ast, Option[Ast]) => Ast)(node: IfExpression): Ast = {
    val conditionAst = astForExpression(node.condition)
    val thenAst      = astForThenClause(node.thenClause)
    val elseAst      = astsForElseClauses(node.elsifClauses, node.elseClause, foldIfExpression(builder))
    builder(node, conditionAst, thenAst, elseAst)
  }

  protected def astForThenClause(node: RubyExpression): Ast = astForStatementList(node.asStatementList)

  private def astsForElseClauses(
    elsIfClauses: List[RubyExpression],
    elseClause: Option[RubyExpression],
    astForIf: IfExpression => Ast
  ): Option[Ast] = {
    elsIfClauses match {
      case Nil => elseClause.map(astForElseClause)
      case elsIfNode :: rest =>
        elsIfNode match {
          case elsIfNode: ElsIfClause =>
            val newIf = IfExpression(elsIfNode.condition, elsIfNode.thenClause, rest, elseClause)(elsIfNode.span)
            val wrappingBlock = blockNode(elsIfNode)
            val wrappedAst    = Ast(wrappingBlock).withChild(astForIf(newIf))
            Some(wrappedAst)
          case elsIfNode =>
            logger.warn(s"Expecting elsif clause in ${code(elsIfNode)} ($relativeFileName), skipping")
            None
        }
    }
  }

  protected def astForStatementList(node: StatementList): Ast = {
    val block = blockNode(node)
    scope.pushNewScope(BlockScope(block))
    val statementAsts = node.statements.flatMap(astsForStatement)
    scope.popScope()
    blockAst(block, statementAsts)
  }

  protected def astForDoBlock(block: Block & RubyExpression): (typeRef: Ast, methodRef: Ast) = {
    if (closureToRefs.contains(block)) {
      val cached = closureToRefs(block).map(x => Ast(x.copy))
      (typeRef = cached(0), methodRef = cached(1))
    } else {
      val methodName = scope.getNewClosureName
      val methodRefAsts = block.body match {
        case x: Block =>
          astForMethodDeclaration(x.toMethodDeclaration(methodName, Option(block.parameters)), isClosure = true)
        case _ =>
          astForMethodDeclaration(block.toMethodDeclaration(methodName, Option(block.parameters)), isClosure = true)
      }
      closureToRefs.put(block, methodRefAsts.flatMap(_.root))
      (typeRef = methodRefAsts(0), methodRef = methodRefAsts(1))
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

    val stmtAsts = node.statements.size match {
      case 0 => List()
      case n =>
        val (headStmts, lastStmt) = node.statements.splitAt(n - 1)
        headStmts.flatMap(astsForStatement) ++ lastStmt.flatMap(astsForImplicitReturnStatement)
    }

    scope.popScope()
    blockAst(block, stmtAsts)
  }

  private def astsForImplicitReturnStatement(node: RubyExpression): Seq[Ast] = {
    def elseReturnNil(span: TextSpan) = Option {
      ElseClause(
        StatementList(
          ReturnExpression(StaticLiteral(Defines.prefixAsCoreType(Defines.NilClass))(span.spanStart("nil")) :: Nil)(
            span.spanStart("return nil")
          ) :: Nil
        )(span.spanStart("return nil"))
      )(span.spanStart("else\n\treturn nil\nend"))
    }

    node match {
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
      case ret: ReturnExpression => astForReturnExpression(ret) :: Nil
      case node: (MethodDeclaration | SingletonMethodDeclaration) =>
        (astsForStatement(node) :+ astForReturnMethodDeclarationSymbolName(node)).toList
      case node: MethodAccessModifier =>
        val simpleIdent = node.toSimpleIdentifier

        val methodIdentName = node.method match {
          case x: StaticLiteral     => x.span.text
          case x: MethodDeclaration => x.methodName
          case x =>
            logger.warn(s"Unknown node type for method identifier name: ${x.getClass} (${this.relativeFileName})")
            x.span.text
        }

        val methodIdent = SimpleIdentifier(None)(simpleIdent.span.spanStart(methodIdentName))

        val simpleCall = SimpleCall(simpleIdent, List(methodIdent))(
          simpleIdent.span.spanStart(s"${simpleIdent.span.text} ${methodIdent.span.text}")
        )
        astForReturnExpression(ReturnExpression(List(simpleCall))(node.span)) :: Nil
      case node: FieldsDeclaration =>
        val nilReturnSpan    = node.span.spanStart("return nil")
        val nilReturnLiteral = StaticLiteral(Defines.prefixAsCoreType(Defines.NilClass))(nilReturnSpan)
        astsForFieldDeclarations(node) ++ astsForImplicitReturnStatement(nilReturnLiteral)
      case node: SingletonClassDeclaration =>
        astForAnonymousTypeDeclaration(node)
        val nilReturnSpan    = node.span.spanStart("return nil")
        val nilReturnLiteral = StaticLiteral(Defines.prefixAsCoreType(Defines.NilClass))(nilReturnSpan)
        astsForImplicitReturnStatement(nilReturnLiteral)
      case node =>
        returnAst(node) :: Nil
    }
  }

  private def returnAst(node: RubyExpression): Ast = {
    val nodeAst = astForExpression(node)
    returnAst(returnNode(node, code(node)), nodeAst :: Nil)
  }

  protected def astForDefaultMultipleAssignmentExpr(node: DefaultMultipleAssignment): Ast = {
    blockAst(
      blockNode(node, code(node), Defines.Any),
      astForDefaultMultipleAssignment(node, isExpression = true).toList
    )
  }

  protected def astForDefaultMultipleAssignment(node: DefaultMultipleAssignment, isExpression: Boolean): Seq[Ast] = {
    if (!isExpression) {
      node.assignments.map(astForExpression)
    } else {
      val assignmentAsts  = mutable.ListBuffer.empty[Ast]
      val returnExprNames = mutable.ListBuffer.empty[String]

      node.assignments.foreach { assignment =>
        if (isSimpleExpression(assignment.lhs)) {
          assignmentAsts += astForExpression(assignment)
          returnExprNames += simpleExpressionName(assignment.lhs)
        } else {
          val tmpName = scope.getNewVarTmp

          val tmpLhsAst = handleVariableOccurrence(tmpName, assignment)
          val rhsAst    = astForExpression(assignment.rhs)
          val tmpAssignCall = callNode(
            assignment,
            s"$tmpName = ${code(assignment.rhs)}",
            Operators.assignment,
            Operators.assignment,
            DispatchTypes.STATIC_DISPATCH
          )
          assignmentAsts += callAst(tmpAssignCall, Seq(tmpLhsAst, rhsAst))

          val lhsAst    = astForExpression(assignment.lhs)
          val tmpRhsAst = handleVariableOccurrence(tmpName, assignment)
          val lhsAssignCall = callNode(
            assignment,
            s"${code(assignment.lhs)} = $tmpName",
            Operators.assignment,
            Operators.assignment,
            DispatchTypes.STATIC_DISPATCH
          )
          assignmentAsts += callAst(lhsAssignCall, Seq(lhsAst, tmpRhsAst))

          returnExprNames += tmpName
        }
      }
      val arrayTmpName = scope.getNewVarTmp

      val allocCall = callNode(node, Operators.alloc, Operators.alloc, Operators.alloc, DispatchTypes.STATIC_DISPATCH)
      val arrayAssignCall = callNode(
        node,
        s"$arrayTmpName = ${Operators.alloc}",
        Operators.assignment,
        Operators.assignment,
        DispatchTypes.STATIC_DISPATCH
      )
      assignmentAsts += callAst(arrayAssignCall, Seq(handleVariableOccurrence(arrayTmpName, node), callAst(allocCall)))

      returnExprNames.toList.zipWithIndex.foreach { case (exprName, idx) =>
        val idxLiteral = literalNode(node, idx.toString, Defines.prefixAsCoreType(Defines.Integer))
        val indexAccessCall =
          callNode(
            node,
            s"$arrayTmpName[$idx]",
            Operators.indexAccess,
            Operators.indexAccess,
            DispatchTypes.STATIC_DISPATCH
          )
        val indexAccessAst =
          callAst(indexAccessCall, Seq(handleVariableOccurrence(arrayTmpName, node), Ast(idxLiteral)))

        val elemAssignCall = callNode(
          node,
          s"$arrayTmpName[$idx] = $exprName",
          Operators.assignment,
          Operators.assignment,
          DispatchTypes.STATIC_DISPATCH
        )
        assignmentAsts += callAst(elemAssignCall, Seq(indexAccessAst, handleVariableOccurrence(exprName, node)))
      }

      assignmentAsts += handleVariableOccurrence(arrayTmpName, node)
      assignmentAsts.toSeq
    }
  }

  private def isSimpleExpression(expr: RubyExpression): Boolean = expr match {
    case _: RubyIdentifier | _: SelfIdentifier => true
    case _: LiteralExpr                        => true
    case _                                     => false
  }

  private def simpleExpressionName(expr: RubyExpression): String = expr match {
    case _: SelfIdentifier    => Defines.Self
    case node: RubyIdentifier => node.span.text
    case node: LiteralExpr    => node.span.text
    case node                 => code(node)
  }

  // The evaluation of a MethodDeclaration returns its name in symbol form.
  // E.g. `def f = 0` ===> `:f`
  private def astForReturnMethodDeclarationSymbolName(node: RubyExpression & ProcedureDeclaration): Ast = {
    val literalNode_ = literalNode(node, s":${node.methodName}", prefixAsCoreType(Defines.Symbol))
    val returnNode_  = returnNode(node, literalNode_.code)
    returnAst(returnNode_, Seq(Ast(literalNode_)))
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
      case InClause(pattern, body) => InClause(pattern, returnLastNode(body, transform))(x.span)
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

  protected def astForAliasStatement(statement: AliasStatement): Seq[Ast] = {
    val aliasMethodDecl = generateAliasMethodDecl(statement)
    // alias should always be lifted to the class decl
    astForMethodDeclaration(aliasMethodDecl, useSurroundingTypeFullName = true)
  }

  private def generateAliasMethodDecl(alias: AliasStatement): MethodDeclaration = {
    val span                 = alias.span
    val forwardingCallTarget = SimpleIdentifier(None)(span.spanStart(alias.oldName))
    val forwardedArgs        = SplattingRubyNode(SimpleIdentifier()(span.spanStart("args")))(span.spanStart("*args"))
    val forwardedBlock       = SimpleIdentifier()(span.spanStart("&block"))
    val forwardingCall = SimpleCall(forwardingCallTarget, forwardedArgs :: forwardedBlock :: Nil)(
      span.spanStart(s"${alias.oldName}(*args, &block)")
    )

    val aliasMethodBody = StatementList(forwardingCall :: Nil)(forwardingCall.span)
    val aliasingMethodParams =
      ArrayParameter("*args")(span.spanStart("*args")) :: ProcParameter("&block")(span.spanStart("&block")) :: Nil

    MethodDeclaration(alias.newName, aliasingMethodParams, aliasMethodBody)(
      alias.span.spanStart(s"def ${alias.newName}(*args, &block)")
    )
  }
}
