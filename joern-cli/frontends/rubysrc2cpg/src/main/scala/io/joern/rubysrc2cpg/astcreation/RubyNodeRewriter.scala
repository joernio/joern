package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.*

import cats.*
import cats.data.*
import cats.syntax.all.*
import cats.instances.*
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.passes.Defines.getBuiltInType

trait RubyNodeRewriter { this: AstCreator =>

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

}
