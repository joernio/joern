package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.*
import io.joern.rubysrc2cpg.datastructures.BlockScope
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.passes.Defines.getBuiltInType
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.nodes.{NewMethod, NewMethodRef, NewTypeDecl}

import cats.*
import cats.data.*
import cats.syntax.all.*
import cats.instances.*

trait AstForStatementsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  type RubyRewrite[T] = Writer[List[RubyNode], T]

  final case class Fresh(name: String, span: TextSpan) {
    def id: SimpleIdentifier = SimpleIdentifier(None)(span.spanStart(name))
    def call: SimpleCall = SimpleCall(id, List())(span.spanStart(name))
    def ref: BlockArgument = BlockArgument(MethodRefTo(call, name))(span.spanStart(name))
  }
  def fresh(span: TextSpan): Fresh = Fresh(tmpGen.fresh, span)

  implicit class RubyRewriteRubyNodeListExt(rr: RubyRewrite[List[RubyNode]]) {
    def rets(span: TextSpan): RubyRewrite[TextSpan] =
      for
        rs <- rr
        _ <- Writer.tell(List(
          ReturnExpression(rs)(span)
        ))
      yield span
  }

  implicit class RubyRewriteRubyNodeExt(rr: RubyRewrite[RubyNode]) {
    def atomic: Boolean = rr.written.isEmpty
    def compound: Boolean = !atomic
    def assign(lhs: RubyNode, op: String = "=", span: Option[TextSpan] = None): RubyRewrite[TextSpan] =
      for
        r <- rr
        _ <- Writer.tell(List(
          SingleAssignment(lhs, op, r)(span.getOrElse(r.span))
        ))
      yield r.span
    def assign(): RubyRewrite[RubyNode] =
      rr.value match {
        case id: SimpleIdentifier =>
          rr
        case _ =>
          val res = fresh(rr.value.span).id
          assign(res).map { _ => res }
      }

    def tuck: RubyRewrite[TextSpan] = 
      for
        r <- rr
        _ <- Writer.tell(List(r))
      yield r.span
    def ret: RubyRewrite[TextSpan] = rr.map(List(_)).rets(rr.value.span)
    def defer(parameters: List[RubyNode] = List()): RubyRewrite[Fresh] = {
      val span = rr.value.span
      val n = fresh(span)
      Writer(
        List(MethodRefFrom(MethodDeclaration(n.name, parameters, rr.ret.body)(span), n.name)),
        n
      )
    }

    def deferCompound: RubyRewrite[RubyNode] =
      if (compound)
        defer().map(_.call)
      else 
        rr
    
  }

  implicit class RubyRewriteUnitExt(rr: RubyRewrite[TextSpan]) {
    def asList: List[RubyNode] = rr.written
    def span: TextSpan = rr.value
    def body: RubyNode = StatementList(asList)(span)
  }

  implicit class TextSpanExt(span: TextSpan) {
    def nilLiteralEnd: RubyNode = StaticLiteral(getBuiltInType(Defines.NilClass))(span.spanEnd("nil"))
  }

  def negate(node: RubyNode): RubyNode = UnaryExpression("!", node)(node.span)

  protected def rewriteNode(node: RubyNode, expectExpression: Boolean): RubyRewrite[RubyNode] = node match {

    case node @ SimpleCall(target, arguments)    => 
      for
        tgt <- rewriteNode(target, true)
        args <- arguments.traverse(rewriteNode(_, true))
      yield SimpleCall(tgt, args)(node.span)

    case node @ SimpleCallWithBlock(target, arguments, block @ Block(parameters, body)) =>
      for
        blk <- rewriteNode(body, true).defer(parameters)
        res <- rewriteNode(SimpleCall(target, arguments :+ blk.ref)(node.span), expectExpression)
      yield res

    // case node: ForExpression              => ???
    case node @ WhileExpression(condition, body)            =>
      for
        cond <- rewriteNode(condition, true).deferCompound
        bdy = rewriteNode(body, expectExpression)
        res <- 
          if (expectExpression)
            val tmp = fresh(body.span).id
            Writer(List(
              WhileExpression(cond, bdy.assign(tmp).body)(node.span)
            ), tmp)
          else
            Writer(List(), WhileExpression(cond, bdy.tuck.body)(node.span))
      yield res
        
    case node @ UntilExpression(cond, body) =>
      rewriteNode(WhileExpression(negate(cond),body)(node.span), expectExpression)
    case node @ IfExpression(cond, thenBody, List(), Some(elseClause @ ElseClause(elseBody))) =>
      for 
        cnd <- rewriteNode(cond, true)
        thn = rewriteNode(thenBody, expectExpression)
        els = rewriteNode(elseBody, expectExpression)
        res <- 
          if (expectExpression)
            val tmp = fresh(cnd.span).id
            Writer(List(
              IfExpression(cond, thn.assign(tmp).body, List(), Some(ElseClause(els.assign(tmp).body)(elseClause.span)))(node.span)
            ), tmp)
          else
            Writer(List(), IfExpression(cond, thn.tuck.body, List(), Some(ElseClause(els.tuck.body)(elseClause.span)))(node.span))
      yield res
    case node @ IfExpression(cond, thenBody, elifClauses, elseClause) => 
      val elseBody = elseClause match {
        case None => node.span.nilLiteralEnd
        case Some(ElseClause(body)) => body
        case _ => ???
      }
      rewriteNode(IfExpression(cond, thenBody, List(), Some(elifClauses.foldRight(elseBody) {
        case (elif @ ElsIfClause(elifCond, elifThen), rest) => IfExpression(elifCond, elifThen, List(), Some(ElseClause(rest)(rest.span)))(elif.span)
        case _ => ???
      }))(node.span), expectExpression)
        
    case node @ UnlessExpression(cond, thenClause, elseClause) =>
      rewriteNode(IfExpression(negate(cond), thenClause, List(), elseClause)(node.span), expectExpression)
    case node @ SingleAssignment(lhs, op, rhs) => 
      for
         _ <- rewriteNode(rhs, true).assign(lhs, op, Some(node.span))
      yield lhs
    case node @ StatementList(Nil)        => rewriteNode(StatementList(List(node.span.nilLiteralEnd))(node.span), expectExpression)
    case node @ StatementList(stmts :+ stmt) => 
      for
        _ <- stmts.traverse_(rewriteNode(_, false).tuck)
        r <- rewriteNode(stmt, true)
      yield r

    case node @ MemberAccess(target, op, name) => rewriteNode(MemberCall(target, op, name, List())(node.span), expectExpression)
    case node @ MemberCall(target, op, name, arguments) =>
      for
        tgt <- rewriteNode(target, true)
        args <- arguments.traverse(rewriteNode(_, true))
      yield MemberCall(tgt, op, name, args)(node.span)
    // case node: MemberCallWithBlock        => ???
    case node @ MemberCallWithBlock(target, op, name, arguments, block @ Block(parameters, body)) =>
      for
        blk <- rewriteNode(body, true).defer(parameters)
        res <- rewriteNode(MemberCall(target, op, name, arguments :+ blk.ref)(node.span), expectExpression)
      yield res
    case node @ ReturnExpression(expressions)           =>
      for
        exprs <- expressions.traverse(rewriteNode(_, true))
      yield ReturnExpression(exprs)(node.span)
    case node @ AnonymousClassDeclaration(name, baseClass, body) =>
      val bdy = rewriteNode(body, false).tuck.body
      AnonymousClassDeclaration(name, baseClass, bdy)(node.span).pure
    case node @ SingletonClassDeclaration(name, baseClass, body) =>
      val bdy = rewriteNode(body, false).tuck.body
      SingletonClassDeclaration(name, baseClass, bdy)(node.span).pure
    case node @ ModuleDeclaration(name, body) =>
      val bdy = rewriteNode(body, false).tuck.body
      ModuleDeclaration(name, bdy)(node.span).pure
    case node @ ClassDeclaration(name, body, baseClass, fields) =>
      val bdy = body.map(rewriteNode(_, false).tuck.body)
      ClassDeclaration(name, bdy, baseClass, fields)(node.span).pure
    case node @ MethodDeclaration(name, parameters, body)          => 
      val bdy = rewriteNode(body, true).ret.body
      MethodDeclaration(name, parameters, bdy)(node.span).pure
    case node @ SingletonMethodDeclaration(target, name, parameters, body) => ???
      val bdy = rewriteNode(body, true).ret.body
      SingletonMethodDeclaration(target, name, parameters, bdy)(node.span).pure
    case node @ MultipleAssignment(assignments) =>
      for
        assigns <- assignments.traverse(rewriteNode(_,false))
      yield MultipleAssignment(assigns.map(_.asInstanceOf[SingleAssignment]))(node.span)

    case node @ UnaryExpression(op, expression)            =>
      rewriteNode(expression, true).map(UnaryExpression(op, _)(node.span))

    case node @ BinaryExpression(lhs, op, rhs)           =>
      for
        l <- rewriteNode(lhs, true)
        r <- rewriteNode(rhs, true)
      yield BinaryExpression(l, op, r)(node.span)
    case node @ SimpleObjectInstantiation(target, arguments) => 
      for
        tgt <- rewriteNode(target, true)
        args <- arguments.traverse(rewriteNode(_, true))
      yield SimpleObjectInstantiation(tgt, args)(node.span)
    case node @ ObjectInstantiationWithBlock(target, arguments, block @ Block(parameters, body)) => ???
      for
        blk <- rewriteNode(body, true).defer(parameters)
        res <- rewriteNode(SimpleObjectInstantiation(target, arguments :+ blk.ref)(node.span), expectExpression)
      yield res
      
    case node @ IndexAccess(target, indices) =>
      rewriteNode(target, expectExpression).map(IndexAccess(_, indices)(node.span))

    case node @ AttributeAssignment(target, op, name, rhs) =>
      for
        tgt <- rewriteNode(target, false) // We don't handle, for example `(if x then y else z).w = v`
        r <- rewriteNode(rhs, true)
      yield AttributeAssignment(tgt, op, name, r)(node.span)
    case node @ RangeExpression(lowerBound, upperBound, op) =>
      for
        lb <- rewriteNode(lowerBound, true)
        ub <- rewriteNode(upperBound, true)
      yield RangeExpression(lb, ub, op)(node.span)
    case node: ArrayLiteral               => node.pure // For now

    case node: HashLiteral                => node.pure // For now
    case node@ Association(key, value)                =>
      for
        k <- rewriteNode(key, true)
        v <- rewriteNode(value, true)
      yield Association(k, v)(node.span)
    case node @ RescueExpression(body, rescueClauses, elseClause, ensureClause) => 
      val res = fresh(node.span).id
      val bdy = rewriteNode(body, expectExpression).assign(res).body
      val rBdys = rescueClauses.map {
        case rescue @ RescueClause(exceptionClassList, assignment, thenBody) => rewriteNode(thenBody, expectExpression).map(RescueClause(exceptionClassList, assignment, _)(rescue.span)).assign(res).body
        case _ => ???
      }
      val elsBdy = elseClause.map {
        case els @ ElseClause(body) => rewriteNode(body, expectExpression).map(ElseClause(_)(els.span)).assign(res).body
        case _ => ???
      }
      val ensBdy = ensureClause.map {
        case ens @ EnsureClause(body) => rewriteNode(body, false).map(EnsureClause(_)(ens.span)).assign(res).body
        case _ => ???
      }
      Writer(List(
        RescueExpression(bdy, rBdys, elsBdy, ensBdy)(node.span)
      ), res)


    case node: CaseExpression             => node.pure // For now
    case node: ProcOrLambdaExpr           => node.pure // For now
    case node: DynamicLiteral             => node.pure // For now

    // case node: SplattingRubyNode          => ???
    // case node: MandatoryParameter         => ???
    // case node: SelfIdentifier             => ???
    // case node: FieldsDeclaration          => ???
    // case node: RequireCall                => ???
    // case node: IncludeCall                => ???
    // case node: DummyNode                  => ???
    // case node: StaticLiteral              => ???
    // case node: HereDocNode                => ???
    // case node: RubyIdentifier             => ???
    // case node: YieldExpr                  => ???
    case _                                => node.pure
  }

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
          //
          // A single match expression is compared using `.===` to the case target expression if it is present
          // otherwise it is treated as a conditional.
          //
          // There may be a splat as the last match expression,
          // `case y when *x then c end` or
          // `case when *x then c end`
          // which is translated to `x.include? y` and `x.any?` conditions respectively

          val conditions = whenClause.matchExpressions.map { mExpr =>
            expr.map(e => MemberCall(mExpr, ".", "===", List(e))(mExpr.span)).getOrElse(mExpr)
          } ++ (whenClause.matchSplatExpression.iterator.flatMap {
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
        val tmp = SimpleIdentifier(None)(e.span.spanStart(tmpGen.fresh))
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
  protected def astsForCallWithBlock[C <: RubyCall](node: RubyNode with RubyCallWithBlock[C]): Seq[Ast] = {
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

    methodDecl :: typeDecl :: callWithLambdaArg :: Nil
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
    def elseReturnNil = Option {
      ElseClause(
        StatementList(
          ReturnExpression(StaticLiteral(getBuiltInType(Defines.NilClass))(node.span.spanStart("nil")) :: Nil)(
            node.span.spanStart("return nil")
          ) :: Nil
        )(node.span.spanStart("return nil"))
      )(node.span.spanStart("else\n\treturn nil\nend"))
    }

    node match
      case expr: ControlFlowExpression =>
        astsForStatement(transformLastRubyNodeInControlFlowExpressionBody(expr, returnLastNode, elseReturnNil))
      case node: MemberCallWithBlock => returnAstForRubyCall(node)
      case node: SimpleCallWithBlock => returnAstForRubyCall(node)
      case _: (LiteralExpr | BinaryExpression | UnaryExpression | SimpleIdentifier | IndexAccess | Association |
            YieldExpr | RubyCall) =>
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
        (astForMethodDeclaration(node) :+ astForReturnMethodDeclarationSymbolName(node)).toList

      case node =>
        logger.warn(
          s"Implicit return here not supported yet: ${node.text} (${node.getClass.getSimpleName}), only generating statement"
        )
        astsForStatement(node).toList
  }

  private def returnAstForRubyCall[C <: RubyCall](node: RubyNode with RubyCallWithBlock[C]): Seq[Ast] = {
    val Seq(methodDecl, typeDecl, callAst) = astsForCallWithBlock(node): @unchecked

    Ast.storeInDiffGraph(methodDecl, diffGraph)
    Ast.storeInDiffGraph(typeDecl, diffGraph)

    returnAst(returnNode(node, code(node)), List(callAst)) :: Nil
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
    *   the RubyNode with an explicit expression
    */
  private def returnLastNode(x: RubyNode): RubyNode = {
    def statementListReturningLastExpression(stmts: List[RubyNode]): List[RubyNode] = stmts match {
      case (head: ControlFlowClause) :: Nil => clauseReturningLastExpression(head) :: Nil
      case (head: ReturnExpression) :: Nil  => head :: Nil
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

  /** @param node
    *   \- Control Flow Expression RubyNode
    * @param transform
    *   \- RubyNode => RubyNode function for transformation on last ruby node
    * @return
    *   RubyNode with transform function applied
    */
  protected def transformLastRubyNodeInControlFlowExpressionBody(
    node: RubyNode with ControlFlowExpression,
    transform: RubyNode => RubyNode,
    defaultElseBranch: Option[ElseClause]
  ): RubyNode = {
    node match {
      case RescueExpression(body, rescueClauses, elseClause, ensureClause) =>
        // Ensure never returns a value, only the main body, rescue & else clauses
        RescueExpression(
          transform(body),
          rescueClauses.map(transform),
          elseClause.map(transform).orElse(defaultElseBranch),
          ensureClause
        )(node.span)
      case WhileExpression(condition, body) => WhileExpression(condition, transform(body))(node.span)
      case UntilExpression(condition, body) => UntilExpression(condition, transform(body))(node.span)
      case IfExpression(condition, thenClause, elsifClauses, elseClause) =>
        IfExpression(
          condition,
          transform(thenClause),
          elsifClauses.map(transform),
          elseClause.map(transform).orElse(defaultElseBranch)
        )(node.span)
      case UnlessExpression(condition, trueBranch, falseBranch) =>
        UnlessExpression(condition, transform(trueBranch), falseBranch.map(transform).orElse(defaultElseBranch))(
          node.span
        )
      case ForExpression(forVariable, iterableVariable, doBlock) =>
        ForExpression(forVariable, iterableVariable, transform(doBlock))(node.span)
      case CaseExpression(expression, whenClauses, elseClause) =>
        CaseExpression(expression, whenClauses.map(transform), elseClause.map(transform).orElse(defaultElseBranch))(
          node.span
        )
    }
  }
}
