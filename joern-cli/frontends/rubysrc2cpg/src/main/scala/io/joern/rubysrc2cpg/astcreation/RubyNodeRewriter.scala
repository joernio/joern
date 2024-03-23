package io.joern.rubysrc2cpg.astcreation

import cats.*
import cats.data.*
import cats.syntax.all.*
import cats.instances.*

import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.*
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.passes.Defines.getBuiltInType
import io.joern.rubysrc2cpg.utils.FreshNameGenerator
import org.slf4j.{Logger, LoggerFactory}

trait RubyNodeRewriter { this: AstCreator =>

  type RubyRewrite[T] = Writer[List[RubyNode], T]

  val rewriteGen = FreshNameGenerator(i => s"<tmp-gen-$i>")
  val lambdaGen = FreshNameGenerator(i => s"<lambda>$i")

  final case class Fresh(name: String, span: TextSpan) {
    def id: SimpleIdentifier = SimpleIdentifier(None)(span.spanStart(name))
    def call: SimpleCall = SimpleCall(id, List())(span.spanStart(name))
    def ref: BlockArgument = BlockArgument(id)(span.spanStart(name))
  }
  def fresh(span: TextSpan): Fresh = Fresh(rewriteGen.fresh, span)
  def fresh(span: TextSpan, expectExpression: Boolean): Option[Fresh] = 
    if(expectExpression)
      Some(Fresh(rewriteGen.fresh, span))
    else
      None
  def freshLam(span: TextSpan): Fresh = Fresh(lambdaGen.fresh, span)

  implicit class RubyRewriteRubyNodeListExt(rr: RubyRewrite[List[RubyNode]]) {
    def rets(span: TextSpan): RubyRewrite[TextSpan] =
      for
        rs <- rr
        retSpan = span.spanStart(s"return ${rs.map(_.span.text).mkString(", ")}")
        _ <- Writer.tell(List(
          ReturnExpression(rs)(retSpan)
        ))
      yield span
  }

  implicit class RubyNodeExt(node: RubyNode) { 
    def symbol(name: String, expectExpression: Boolean): RubyRewrite[RubyNode] =
      if(expectExpression) {
        Writer(List(node), node.span.symbolLiteralBegin(name))
      } else {
        node.pure
      }

    def getUnqualifiedName: String = "<placeholder>" // TODO
    def isAssignable = node match {
      case _: ReturnExpression => false
      case _ => true
    }
  }

  implicit class RubyRewriteRubyNodeExt(rr: RubyRewrite[RubyNode]) {
    def atomic: Boolean = rr.written.isEmpty
    def compound: Boolean = !atomic
    def assign(lhs: RubyNode, op: String = "=", span: Option[TextSpan] = None): RubyRewrite[TextSpan] =
      for
        r <- rr
        _ <- Writer.tell(List(
          if (r.isAssignable)
            SingleAssignment(lhs, op, r)(span.getOrElse(r.span))
          else
            r
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
      val n = freshLam(span)
      Writer(
        List(MethodDeclaration(n.name, parameters, rr.ret.body, isClosure = true)(span)),
        n
      )
    }

    def deferCompound: RubyRewrite[RubyNode] =
      if (compound)
        defer().map(_.call)
      else 
        rr
    
  }

  implicit class RubyFlattenExt(node: RubyNode) {
    def extractStmts: Either[RubyNode, List[RubyNode]] = node match {
      case StatementList(stmts) => Right(stmts)
      case _ => Left(node)
    }

    def pluck: RubyNode = node.extractStmts.fold(x => x, {
      case List(x) => x
      case x => node
    })

    def flatten: List[RubyNode] = node.extractStmts.fold(List(_), _.flatMap(_.flatten))
  }

  implicit class OptionFreshExt(fresh: Option[Fresh]) {
    def assignTo(rr: RubyRewrite[RubyNode]): RubyRewrite[TextSpan] = 
      fresh.map { tmp =>
        rr.assign(tmp.id)
      }.getOrElse { rr.tuck }
    def emit(node: RubyNode): RubyRewrite[RubyNode] =
      fresh.map { tmp =>
        Writer(List(node), tmp.id)
      }.getOrElse {
        node.pure
      }
  }

  implicit class RubyRewriteUnitExt(rr: RubyRewrite[TextSpan]) {
    def asList: List[RubyNode] = rr.written.flatMap(_.flatten)
    def span: TextSpan = rr.value
    def body: RubyNode = StatementList(asList)(span)
  }

  implicit class TextSpanExt(span: TextSpan) {
    def nilLiteralEnd: RubyNode = StaticLiteral(getBuiltInType(Defines.NilClass))(span.spanEnd("nil"))
    def symbolLiteralBegin(text: String): RubyNode = StaticLiteral(getBuiltInType(Defines.Symbol))(span.spanStart(s":${text}"))
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

    case node @ WhileExpression(cond, body)            =>
      for
        cnd <- rewriteNode(cond, true).deferCompound
        bdy = rewriteNode(body, expectExpression)
        tmp = fresh(body.span, expectExpression)
        res <- tmp.emit(WhileExpression(cnd, tmp.assignTo(bdy).body)(node.span))
      yield res
        
    case node @ UntilExpression(cond, body) =>
      rewriteNode(WhileExpression(negate(cond),body)(node.span), expectExpression)
    case node @ IfExpression(cond, thenBody, List(), elseClauses) =>
      for 
        cnd <- rewriteNode(cond, true)
        tmp = fresh(node.span, expectExpression)
        thn = tmp.assignTo(rewriteNode(thenBody, expectExpression)).body
        els = elseClauses.map {
          case elseClause @ ElseClause(elseBody) =>
            ElseClause(tmp.assignTo(rewriteNode(elseBody, expectExpression)).body)(elseClause.span)
          case clause => 
            logger.warn(s"Expected else body, got [${clause}]")
            ElseClause(Unknown()(clause.span))(clause.span)
        } match {
          case e if expectExpression => 
            e.orElse { 
              val span = node.span.spanEnd("else nil end")
              Some(ElseClause(node.span.nilLiteralEnd)(span))
            }
          case e => e
        }
        res <- tmp.emit(
          IfExpression(cond, thn, List(), els)(node.span)
        )
      yield res

    case node @ IfExpression(cond, thenBody, elifClauses, elseClauses) => 
      rewriteNode(IfExpression(cond, thenBody, List(), elifClauses.foldRight(elseClauses) {
        case (elif @ ElsIfClause(elifCond, elifThen), rest) => Some(ElseClause(IfExpression(elifCond, elifThen, List(), rest)(elif.span))(elif.span))
        case (node, _) =>
          logger.warn("Expected elif clause")
          Some(Unknown()(node.span))

      })(node.span), expectExpression)
        
    case node @ UnlessExpression(cond, thenClause, elseClause) =>
      rewriteNode(IfExpression(negate(cond), thenClause, List(), elseClause)(node.span), expectExpression)
    case node @ SingleAssignment(lhs, op, rhs) => 
      for
         r <- rewriteNode(rhs, true)
      yield SingleAssignment(lhs, op, r)(node.span)
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
      AnonymousClassDeclaration(name, baseClass, bdy)(node.span).symbol(name.getUnqualifiedName, expectExpression)
    case node @ SingletonClassDeclaration(name, baseClass, body) =>
      val bdy = rewriteNode(body, false).tuck.body
      SingletonClassDeclaration(name, baseClass, bdy)(node.span).symbol(name.getUnqualifiedName, expectExpression)
    case node @ ModuleDeclaration(name, body) =>
      val bdy = rewriteNode(body, false).tuck.body
      ModuleDeclaration(name, bdy)(node.span).symbol(name.getUnqualifiedName, expectExpression)
    case node @ ClassDeclaration(name, baseClass, body,  fields) =>
      val bdy = rewriteNode(body, false).tuck.body
      ClassDeclaration(name, baseClass, body, fields)(node.span).symbol(name.getUnqualifiedName, expectExpression)
    case node @ MethodDeclaration(name, parameters, body, isClosure) => 
      val bdy = rewriteNode(body, true).ret.body
      MethodDeclaration(name, parameters, bdy, isClosure)(node.span).symbol(name, expectExpression)
    case node @ SingletonMethodDeclaration(target, name, parameters, body) =>
      val bdy = rewriteNode(body, true).ret.body
      SingletonMethodDeclaration(target, name, parameters, bdy)(node.span).symbol(name, expectExpression)
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
    case node @ ObjectInstantiationWithBlock(target, arguments, block @ Block(parameters, body)) =>
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
    case node @ RescueExpression(body, rescueClauses, elseClause, ensureClauses) => 
      val tmp = if(expectExpression)
        Some(fresh(node.span))
      else
        None
      val bdy = tmp.assignTo(rewriteNode(body, expectExpression)).body
      val rClauses = rescueClauses.map {
        case rescue @ RescueClause(exceptionClassList, assignment, thenBody) => 
          val rBdy = rewriteNode(thenBody, expectExpression)
          RescueClause(exceptionClassList, assignment, tmp.assignTo(rBdy).body)(rescue.span)
        case node =>
          logger.warn("Expected rescue clause")
          Unknown()(node.span)
      }
      val elsClauses = elseClause.map {
        case els @ ElseClause(body) => 
          val elsBdy = rewriteNode(body, expectExpression)
          ElseClause(tmp.assignTo(elsBdy).body)(els.span)
        case node =>
          logger.warn("Expected else clause")
          Unknown()(node.span)
      } match {
          case e if expectExpression => 
            e.orElse { 
              val span = node.span.spanEnd("else nil end")
              Some(ElseClause(node.span.nilLiteralEnd)(span))
            }
          case e => e
      }
      val ensClauses = ensureClauses.map {
        case ens @ EnsureClause(body) => 
          EnsureClause(rewriteNode(body, false).tuck.body)(ens.span)
        case node =>
          logger.warn("Expected ensure clause")
          Unknown()(node.span)
      }
      tmp.emit(RescueExpression(bdy, rClauses, elsClauses, ensClauses)(node.span))

    case node @ ForExpression(variable, iterable, body) => 
      for
        iter <- rewriteNode(iterable, true)
        bdy = rewriteNode(body, expectExpression)
        tmp = fresh(body.span, expectExpression)
        res <- tmp.emit(ForExpression(variable, iter, tmp.assignTo(bdy).body)(node.span))
      yield res

    case node: CaseExpression             => node.pure // For now
    case node: ProcOrLambdaExpr           => node.pure // For now
    case node: DynamicLiteral             => node.pure // For now
    case node: SplattingRubyNode          => node.pure // For now

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
