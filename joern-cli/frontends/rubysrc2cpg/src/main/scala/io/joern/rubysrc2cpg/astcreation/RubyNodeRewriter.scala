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

object RubyNodeRewriter {

  protected val logger: Logger = LoggerFactory.getLogger(getClass)

  type RubyRewrite[T] = Writer[List[RubyNode], T]

  val rewriteGen = FreshNameGenerator(i => s"<tmp-gen-$i>", raw"<tmp-gen-(\d+)".r)
  val lambdaGen = FreshNameGenerator(i => s"<lambda>$i")

  final case class Fresh(name: String, span: TextSpan) {
    def id: SimpleIdentifier = SimpleIdentifier(None)(span.spanStart(name))
    def call: SimpleCall = SimpleCall(id, List())(span.spanStart(name))
    def ref: BlockArgument = BlockArgument(id)(span.spanStart(name))
  }
  def fresh(span: TextSpan): Option[Fresh] = Some(Fresh(rewriteGen.fresh, span))
  def freshLam(span: TextSpan): Fresh = Fresh(lambdaGen.fresh, span)

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

  implicit class RubyRewriteRubyNodeListExt(rr: RubyRewrite[List[RubyNode]]) {
    def rets(span: TextSpan): RubyRewrite[TextSpan] =
      for
        rs <- rr
        retSpan = span
        _ <- Writer.tell(List(
          ReturnExpression(rs)(retSpan)
        ))
      yield span
  }

  implicit class RubyNodeExt(node: RubyNode) { 
    def symbol(name: String, tmp: Option[Fresh]): RubyRewrite[RubyNode] =
      if(tmp.isDefined) {
        Writer(List(node), node.span.symbolLiteralBegin(name))
      } else {
        node.pure
      }

    def getUnqualifiedName: String = "<placeholder>" // TODO
    def isAssignable = node match {
      case _: ReturnExpression => false
      case _ => true
    }

    // def extractLhs(node: RubyNode): Option[(RubyNode, RubyNode => RubyNode)] = node match {
    //   case node @ IndexAccess(target, indices) => Some((target, IndexAccess(_,indices)(node.span)))
    //   case node @ IndexAccess(target, indices) => Some((target, IndexAccess(_,indices)(node.span)))
    //   case _ => None
    // }
  }

  implicit class RubyRewriteRubyNodeExt(rr: RubyRewrite[RubyNode]) {
    def atomic: Boolean = rr.written.isEmpty
    def compound: Boolean = !atomic
    def assign(lhs: RubyNode, op: String = "=", span: Option[TextSpan] = None): RubyRewrite[TextSpan] =
      for
        rhs <- rr
        _ <- Writer.tell((lhs, rhs) match
          // We skip assignments of identifcal temporary variables
          case (RubyNode.Span(rewriteGen.extract(i), SimpleIdentifier(_)), RubyNode.Span(rewriteGen.extract(j), SimpleIdentifier(_))) if i == j => List()
          case (l, r) if r.isAssignable => List(SingleAssignment(lhs, op, rhs)(span.getOrElse(r.span)))
          case (_, r) => List(r)
        )
      yield rhs.span

    def tuck: RubyRewrite[TextSpan] = 
      for
        r <- rr
        _ <- Writer.tell(List(r))
      yield r.span
    def ret: RubyRewrite[TextSpan] = 
      if (rr.value.isAssignable)
        rr.map(List(_)).rets(rr.value.span)
      else
        rr.tuck
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

  protected def rewriteExpr(node: RubyNode): RubyRewrite[RubyNode] = rewriteNode(node, fresh(node.span))

  protected def rewriteNode(node: RubyNode, tmp: Option[Fresh]): RubyRewrite[RubyNode] = node match {

    case node @ SimpleCall(target, arguments)    => 
      for
        tgt <- rewriteExpr(target)
        args <- arguments.traverse(rewriteExpr(_))
      yield SimpleCall(tgt, args)(node.span)

    case node @ SimpleCallWithBlock(target, arguments, block @ Block(parameters, body)) =>
      for
        blk <- rewriteExpr(body).defer(parameters)
        res <- rewriteNode(SimpleCall(target, arguments :+ blk.ref)(node.span), tmp)
      yield res

    case node @ WhileExpression(cond, body)            =>
      for
        cnd <- rewriteExpr(cond).deferCompound
        bdy = rewriteNode(body, tmp)
        res <- tmp.emit(WhileExpression(cnd, tmp.assignTo(bdy).body)(node.span))
      yield res
        
    case node @ UntilExpression(cond, body) =>
      rewriteNode(WhileExpression(negate(cond),body)(node.span), tmp)
    case node @ IfExpression(cond, thenBody, List(), elseClauses) =>
      for 
        cnd <- rewriteExpr(cond)
        thn = tmp.assignTo(rewriteNode(thenBody, tmp)).body
        els = elseClauses.map {
          case elseClause @ ElseClause(elseBody) =>
            ElseClause(tmp.assignTo(rewriteNode(elseBody, tmp)).body)(elseClause.span)
          case clause => 
            logger.warn(s"Expected else body, got [${clause}]")
            ElseClause(Unknown()(clause.span))(clause.span)
        } match {
          case e if tmp.isDefined => 
            e.orElse { 
              val spanNil = node.span.nilLiteralEnd
              val span = node.span.spanEnd(s"else ${spanNil.span.text} end")
              Some(ElseClause(tmp.assignTo(spanNil.pure).body)(span))
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
      })(node.span), tmp)
        
    case node @ UnlessExpression(cond, thenClause, elseClause) =>
      rewriteNode(IfExpression(negate(cond), thenClause, List(), elseClause)(node.span), tmp)
    case node @ SingleAssignment(lhs, op, rhs) => 
      for
         r <- rewriteExpr(rhs)
      yield SingleAssignment(lhs, op, r)(node.span)
    case node @ StatementList(Nil)        => rewriteNode(StatementList(List(node.span.nilLiteralEnd))(node.span), tmp)
    case node @ StatementList(stmts :+ stmt) => 
      for
        _ <- stmts.traverse_(rewriteNode(_, None).tuck)
        r <- rewriteNode(stmt, tmp)
      yield r

    case node @ MemberAccess(target, op, name) => rewriteNode(MemberCall(target, op, name, List())(node.span), tmp)
    case node @ MemberCall(target, op, name, arguments) =>
      for
        tgt <- rewriteExpr(target)
        args <- arguments.traverse(rewriteExpr(_))
      yield MemberCall(tgt, op, name, args)(node.span)
    case node @ MemberCallWithBlock(target, op, name, arguments, block @ Block(parameters, body)) =>
      for
        blk <- rewriteExpr(body).defer(parameters)
        res <- rewriteNode(MemberCall(target, op, name, arguments :+ blk.ref)(node.span), tmp)
      yield res
    case node @ ReturnExpression(expressions)           =>
      for
        exprs <- expressions.traverse(rewriteExpr(_))
      yield ReturnExpression(exprs)(node.span)
    case node @ AnonymousClassDeclaration(name, baseClass, body) =>
      val bdy = rewriteNode(body, None).tuck.body
      AnonymousClassDeclaration(name, baseClass, bdy)(node.span).pure
    case node @ SingletonClassDeclaration(name, baseClass, body) =>
      val bdy = rewriteNode(body, None).tuck.body
      SingletonClassDeclaration(name, baseClass, bdy)(node.span).symbol(name.getUnqualifiedName, tmp)
    case node @ ModuleDeclaration(name, body) =>
      val bdy = rewriteNode(body, None).tuck.body
      ModuleDeclaration(name, bdy)(node.span).symbol(name.getUnqualifiedName, tmp)
    case node @ ClassDeclaration(name, baseClass, body,  fields) =>
      val bdy = rewriteNode(body, None).tuck.body
      ClassDeclaration(name, baseClass, body, fields)(node.span).symbol(name.getUnqualifiedName, tmp)
    case node @ MethodDeclaration(name, parameters, body, isClosure) => 
      val bdy = rewriteExpr(body).ret.body
      MethodDeclaration(name, parameters, bdy, isClosure)(node.span).symbol(name, tmp)
    case node @ SingletonMethodDeclaration(target, name, parameters, body) =>
      val bdy = rewriteExpr(body).ret.body
      SingletonMethodDeclaration(target, name, parameters, bdy)(node.span).symbol(name, tmp)
    case node @ MultipleAssignment(assignments) =>
      for
        assigns <- assignments.traverse(rewriteNode(_,None))
      yield MultipleAssignment(assigns.map(_.asInstanceOf[SingleAssignment]))(node.span)

    case node @ UnaryExpression(op, expression)            =>
      rewriteExpr(expression).map(UnaryExpression(op, _)(node.span))

    case node @ BinaryExpression(lhs, op, rhs)           =>
      for
        l <- rewriteExpr(lhs)
        r <- rewriteExpr(rhs)
      yield BinaryExpression(l, op, r)(node.span)
    case node @ SimpleObjectInstantiation(target, arguments) => 
      for
        tgt <- rewriteExpr(target)
        args <- arguments.traverse(rewriteExpr(_))
      yield SimpleObjectInstantiation(tgt, args)(node.span)
    case node @ ObjectInstantiationWithBlock(target, arguments, block @ Block(parameters, body)) =>
      for
        blk <- rewriteExpr(body).defer(parameters)
        res <- rewriteNode(SimpleObjectInstantiation(target, arguments :+ blk.ref)(node.span), tmp)
      yield res
      
    case node @ IndexAccess(target, indices) =>
      rewriteNode(target, tmp).map(IndexAccess(_, indices)(node.span))

    case node @ AttributeAssignment(target, op, name, rhs) =>
      for
        tgt <- rewriteNode(target, None) // We don't handle, for example `(if x then y else z).w = v`
        r <- rewriteExpr(rhs)
      yield AttributeAssignment(tgt, op, name, r)(node.span)
    case node @ RangeExpression(lowerBound, upperBound, op) =>
      for
        lb <- rewriteExpr(lowerBound)
        ub <- rewriteExpr(upperBound)
      yield RangeExpression(lb, ub, op)(node.span)
    case node: ArrayLiteral               => node.pure // For now

    case node: HashLiteral                => node.pure // For now
    case node@ Association(key, value)                =>
      for
        k <- rewriteExpr(key)
        v <- rewriteExpr(value)
      yield Association(k, v)(node.span)
    case node @ RescueExpression(body, rescueClauses, elseClause, ensureClauses) => 
      val bdy = tmp.assignTo(rewriteNode(body, tmp)).body
      val rClauses = rescueClauses.map {
        case rescue @ RescueClause(exceptionClassList, assignment, thenBody) => 
          val rBdy = rewriteNode(thenBody, tmp)
          RescueClause(exceptionClassList, assignment, tmp.assignTo(rBdy).body)(rescue.span)
        case node =>
          logger.warn("Expected rescue clause")
          Unknown()(node.span)
      }
      val elsClauses = elseClause.map {
        case els @ ElseClause(body) => 
          val elsBdy = rewriteNode(body, tmp)
          ElseClause(tmp.assignTo(elsBdy).body)(els.span)
        case node =>
          logger.warn("Expected else clause")
          Unknown()(node.span)
      } match {
          case e if tmp.isDefined => 
            e.orElse { 
              val span = node.span.spanEnd("else nil end")
              Some(ElseClause(node.span.nilLiteralEnd)(span))
            }
          case e => e
      }
      val ensClauses = ensureClauses.map {
        case ens @ EnsureClause(body) => 
          EnsureClause(rewriteNode(body, None).tuck.body)(ens.span)
        case node =>
          logger.warn("Expected ensure clause")
          Unknown()(node.span)
      }
      tmp.emit(RescueExpression(bdy, rClauses, elsClauses, ensClauses)(node.span))

    case node @ ForExpression(variable, iterable, body) => 
      for
        iter <- rewriteExpr(iterable)
        bdy = rewriteNode(body, tmp)
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
    // case node: YieldExpr                  => ???
    case _                                => node.pure
  }

  def freshId(span: TextSpan): RubyNode = Fresh(rewriteGen.fresh, span).id
  
  final case class Log(val stmts: Chain[RubyNode], val hasSplit: Boolean)
  implicit val logMonoid: Monoid[Log] = new Monoid[Log] {
    override def empty = Log(Chain.empty, false)
    override def combine(a: Log, b: Log): Log = Log(a.stmts combine b.stmts, a.hasSplit || b.hasSplit)
  }

  type Lowering[T] = WriterT[Eval,Log,T]
  type LoweringCont[T] = ContT[Lowering,RubyNode,T]

  object Log {
    def setSplit[T]: Lowering[Unit] = WriterT.tell(Log(Chain.empty, true))
    def stmt[T](nodes: RubyNode*): Lowering[Unit] = WriterT.tell(Log(Chain(nodes*), false))
  }

  implicit class LoweringExt[T](l: Lowering[T]) {
    def lift: LoweringCont[T] = ContT.liftF(l)
    def log: Log = l.written.value
    def stmts: List[RubyNode] = log.stmts.toList.flatMap(_.flatten)
    def extract: T = l.value.value
  }

  implicit class LoweringRubyNodeExt(l : Lowering[RubyNode]) {
    def span: TextSpan = l.value.value.span
    def tuck: Lowering[TextSpan] = 
      for
        n <- l
        _ <- Log.stmt(n)
      yield n.span
  }

  implicit class LoweringTextSpanExt(l : Lowering[TextSpan]) {
    def body: RubyNode = StatementList(l.stmts)(l.extract)
  }

  implicit class TraverseLoweringContRubyNodeExt[T[_]](lc: T[LoweringCont[RubyNode]])(implicit traverse: Traverse[T]) {
    /** Checks if ant of the elements of l will split. If so assign then temporary variables to preserve argument order.
     *  Otherwise pass them as is.
     */
    def tmpify: LoweringCont[T[RubyNode]] = ContT.apply { next =>
      val l = lc.traverse(_.lowering)
      if(l.log.hasSplit)
        println("GOT HERE 1")
        lc.sequence.run(node => 
          for
            n <- next(node)
            tmp = freshId(n.span)
            _ <- Log.stmt(SingleAssignment(tmp, "=", n)(n.span))
            _ = println("GOT HERE 2")
          yield tmp
            
        )
      else
        lc.sequence.run(next)
        
    }
  }


  implicit class LoweringContRubyNodeExt(l: LoweringCont[RubyNode]) {
    def tmpify: LoweringCont[RubyNode] = TraverseLoweringContRubyNodeExt[Id](l).tmpify

    def lowering: Lowering[RubyNode] = l.run(x => x.pure)
  }


  abstract class Context {
    def noLhs: Context = this match {
      case LhsContext() => ExprContext()
      case ctxt => ctxt
    }
    def noStmt: Context = this match {
      case StmtContext() => ExprContext()
      case ctxt => ctxt
    }
  }
  final case class StmtContext() extends Context
  final case class ExprContext() extends Context
  final case class LhsContext() extends Context


  def lower(node: RubyNode, ctxt: Context): LoweringCont[RubyNode] = node match {
    case node @ IfExpression(cond, thenBody, List(), elseClauses) => ContT.apply { next =>
      for
        cnd <- lower(cond, ExprContext()).tmpify.lowering
        thn = lower(thenBody, ctxt.noLhs).run(next).tuck.body
        els = elseClauses.map {
          case elseClause @ ElseClause(elseBody) =>
            ElseClause(lower(elseBody, ctxt.noLhs).run(next).tuck.body)(elseClause.span)
          case clause => 
            logger.warn(s"Expected else body, got [${clause}]")
            ElseClause(Unknown()(clause.span))(clause.span)
        } match {
          case e if ctxt == ExprContext() => 
            e.orElse { 
              val spanNil = node.span.nilLiteralEnd
              val span = node.span.spanEnd(s"else ${spanNil.span.text} end")
              Some(ElseClause(next(spanNil).tuck.body)(span))
            }
          case e => e
        }
        _ <- Log.setSplit
        _ = println("GOT HERE 3")
      yield IfExpression(cnd, thn, List(), els)(node.span)
    }
    case node @ IfExpression(cond, thenBody, elifClauses, elseClauses) => 
      lower(IfExpression(cond, thenBody, List(), elifClauses.foldRight(elseClauses) {
        case (elif @ ElsIfClause(elifCond, elifThen), rest) => Some(ElseClause(IfExpression(elifCond, elifThen, List(), rest)(elif.span))(elif.span))
        case (node, _) =>
          logger.warn("Expected elif clause")
          Some(Unknown()(node.span))
      })(node.span), ctxt)

    case node @ UnlessExpression(cond, thenClause, elseClause) =>
      lower(IfExpression(negate(cond), thenClause, List(), elseClause)(node.span), ctxt)

    case node @ SimpleCall(target, arguments) =>
      for
        expr <- Tuple2K(target: Id[RubyNode], arguments).map(lower(_,ExprContext())).tmpify
      yield expr match {
        case Tuple2K(tgt, args) => SimpleCall(tgt, args)(node.span)
      }

    case node @ MemberCall(target, op, name, arguments) =>
      for
        exprs <- ctxt match {
          case LhsContext() =>
            Tuple2K(lower(target, ExprContext()): Id[LoweringCont[RubyNode]], arguments.map(lower(_,ExprContext()).tmpify)).sequence
          case _ => 
            Tuple2K(target: Id[RubyNode], arguments).map(lower(_,ExprContext())).tmpify
        }
      yield 
        exprs match { case Tuple2K(tgt, args) => MemberCall(tgt, op, name, args)(node.span) }

    case node @ MemberAccess(target, op, name) => 
      lower(MemberCall(target, op, name, List())(node.span), ctxt)

    case node @ IndexAccess(target, indices) =>
      for
        exprs <- ctxt match {
          case LhsContext() =>
            Tuple2K(lower(target, ExprContext()): Id[LoweringCont[RubyNode]], indices.map(lower(_,ExprContext()).tmpify)).sequence
          case _ => 
            Tuple2K(target: Id[RubyNode], indices).map(lower(_,ExprContext())).tmpify
        }
      yield 
        exprs match { case Tuple2K(tgt, idxs) => IndexAccess(tgt, idxs)(node.span) }

    case node @ SingleAssignment(lhs, op, rhs) =>
      for
        l <- lower(lhs, LhsContext())
        r <- lower(rhs, ExprContext())
      yield SingleAssignment(l, op, r)(node.span)

    case node @ AttributeAssignment(lhs, op, name, rhs) =>
      lower(MemberCall(lhs, op, s"${name}=", List(rhs))(node.span), LhsContext())

    case node @ StatementList(Nil)        => lower(StatementList(List(node.span.nilLiteralEnd))(node.span), ctxt)
    case node @ StatementList(stmts :+ stmt) => ContT.apply { next =>
      for
        _ <- stmts.traverse_(lower(_, StmtContext()).lowering.tuck)
        r <- lower(stmt, ctxt).run(next)
      yield r
    }

    case node => node.pure
  }

}
