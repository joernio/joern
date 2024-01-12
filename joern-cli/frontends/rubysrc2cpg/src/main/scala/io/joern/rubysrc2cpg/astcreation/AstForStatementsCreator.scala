package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.*
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.passes.Defines.getBuiltInType
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes

trait AstForStatementsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astsForStatement(node: RubyNode): Seq[Ast] = node match
    case node: WhileExpression            => astForWhileStatement(node) :: Nil
    case node: UntilExpression            => astForUntilStatement(node) :: Nil
    case node: IfExpression               => astForIfStatement(node) :: Nil
    case node: UnlessExpression           => astForUnlessStatement(node) :: Nil
    case node: CaseExpression             => astsForCaseExpression(node)
    case node: StatementList              => astForStatementList(node) :: Nil
    case node: SimpleCallWithBlock        => astForSimpleCallWithBlock(node) :: Nil
    case node: MemberCallWithBlock        => astForMemberCallWithBlock(node) :: Nil
    case node: ReturnExpression           => astForReturnStatement(node) :: Nil
    case node: ModuleDeclaration          => astForModuleDeclaration(node) :: Nil
    case node: ClassDeclaration           => astForClassDeclaration(node) :: Nil
    case node: FieldsDeclaration          => astsForFieldDeclarations(node)
    case node: MethodDeclaration          => astForMethodDeclaration(node) :: Nil
    case node: SingletonMethodDeclaration => astForSingletonMethodDeclaration(node) :: Nil
    case _                                => astForExpression(node) :: Nil

  private def astForWhileStatement(node: WhileExpression): Ast = {
    val conditionAst = astForExpression(node.condition)
    val bodyAsts     = astsForStatement(node.body)
    whileAst(Some(conditionAst), bodyAsts)
  }

  // `until T do B` is lowered as `while !T do B`
  private def astForUntilStatement(node: UntilExpression): Ast = {
    val notCondition = astForExpression(UnaryExpression("!", node.condition)(node.condition.span))
    val bodyAsts     = astsForStatement(node.body)
    whileAst(Some(notCondition), bodyAsts)
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
        val tmp = SimpleIdentifier(None)(e.span.spanStart(freshName))
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
    scope.pushNewScope(block)
    val statementAsts = node.statements.flatMap(astsForStatement)
    scope.popScope()
    blockAst(block, statementAsts)
  }

  /* `foo(<args>) do <params> <stmts> end` is lowered as a BLOCK node shaped like so:
   * ```
   * {
   *   <params> = foo(<args>)
   *   <stmts>
   * }
   * ```
   * If <params> is empty, we simply exclude the initial assignment (but keep the invocation)
   * TODO: this representation is not final. A better one is to more closely resemble Ruby's semantics
   *  and pass in the block (a closure) as an argument to `foo`, i.e. `foo(<args>, <block>)`.
   */
  private def astForSimpleCallWithBlock(node: SimpleCallWithBlock): Ast = {
    val rubyBlock   = node.block.asInstanceOf[Block]
    val blockParams = rubyBlock.parameters
    if (blockParams.nonEmpty) {
      logger.warn(s"Blocks with parameters are not supported yet: ${code(node)} ($relativeFileName), skipping")
      astForUnknown(node)
    } else {
      val outerBlock = blockNode(node)
      val callAst    = astForSimpleCall(node.withoutBlock)
      methodAstParentStack.push(outerBlock)
      scope.pushNewScope(outerBlock)
      val stmtAsts = rubyBlock.body match
        case stmtList: StatementList => stmtList.statements.flatMap(astsForStatement)
        case body =>
          logger.warn(s"Non-linear method bodies are not supported yet: ${body.text} ($relativeFileName), skippipg")
          astForUnknown(body) :: Nil
      scope.popScope()
      methodAstParentStack.pop()
      blockAst(outerBlock, callAst :: stmtAsts)
    }
  }

  private def astForMemberCallWithBlock(node: MemberCallWithBlock): Ast = {
    val rubyBlock   = node.block.asInstanceOf[Block]
    val blockParams = rubyBlock.parameters
    if (blockParams.nonEmpty) {
      logger.warn(s"Blocks with parameters are not supported yet: ${code(node)}, skipping")
      astForUnknown(node)
    } else {
      val outerBlock = blockNode(node)
      val callAst    = astForMemberCall(node.withoutBlock)
      methodAstParentStack.push(outerBlock)
      scope.pushNewScope(outerBlock)
      val stmtAsts = rubyBlock.body match
        case stmtList: StatementList => stmtList.statements.flatMap(astsForStatement)
        case body =>
          logger.warn(s"Non-linear method bodies are not supported yet: ${body.text}, skipping")
          astForUnknown(body) :: Nil
      scope.popScope()
      methodAstParentStack.pop()
      blockAst(outerBlock, callAst :: stmtAsts)
    }
  }

  protected def astForReturnStatement(node: ReturnExpression): Ast = {
    val argumentAsts = node.expressions.map(astForExpression)
    val returnNode_  = returnNode(node, code(node))
    returnAst(returnNode_, argumentAsts)
  }

  protected def astForStatementListReturningLastExpression(node: StatementList): Ast = {
    val block = blockNode(node)
    scope.pushNewScope(block)

    val stmtAsts = node.statements.size match
      case 0 => List()
      case n =>
        val (headStmts, lastStmt) = node.statements.splitAt(n - 1)
        headStmts.flatMap(astsForStatement) ++ lastStmt.flatMap(astsForImplicitReturnStatement)

    scope.popScope()
    blockAst(block, stmtAsts)
  }

  private def astsForImplicitReturnStatement(node: RubyNode): List[Ast] = {
    node match
      case _: (ArrayLiteral | HashLiteral | StaticLiteral | BinaryExpression | UnaryExpression | SimpleIdentifier |
            IfExpression | RescueExpression | SimpleCall) =>
        astForReturnStatement(ReturnExpression(List(node))(node.span)) :: Nil
      case node: SingleAssignment =>
        astForSingleAssignment(node) :: List(astForReturnStatement(ReturnExpression(List(node.lhs))(node.span)))
      case node: AttributeAssignment =>
        List(
          astForAttributeAssignment(node),
          astForReturnFieldAccess(MemberAccess(node.target, node.op, node.attributeName)(node.span))
        )
      case node: MemberAccess    => astForReturnMemberCall(node) :: Nil
      case ret: ReturnExpression => astForReturnStatement(ret) :: Nil
      case node: MethodDeclaration =>
        List(astForMethodDeclaration(node), astForReturnMethodDeclarationSymbolName(node))
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
}
