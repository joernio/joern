package io.joern.erlang2cpg.ast

import io.joern.erlang2cpg.parser.Term
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.NewLocal

/** OTP catch Expr. An explicit raise transfers its class-specific value to the nearest protected region. Exceptions
  * raised inside an arbitrary callee have no interprocedural exceptional successor in Joern's default CFG. The catch
  * operator records this boundary; its operand is still an ordinary, source-located executable AST.
  */
private[ast] trait ErlangCatch { this: ModuleAst =>
  private case class Region(name: String, slot: NewLocal, end: String)
  private var current: Option[Region] = None

  private def withRegion[A](region: Option[Region])(body: => A): A = {
    val previous = current
    current = region
    try body
    finally current = previous
  }

  def withoutCatch[A](body: => A): A = withRegion(None)(body)

  def catchExpr(term: Term, scope: Scope): Ast = {
    val name   = fresh("catchResult")
    val region = Region(name, local(term, name, scope), fresh("catchEnd"))
    val body   = withRegion(Some(region))(expr(term(2), scope.child()))
    val normal = operator(term, "<operator>.erlangCatch", Seq(body), code(term))
    addModelTag(
      normal,
      "erlang.catch.v1",
      "normal:operand;throw:reason;exit:{'EXIT',reason};error:{'EXIT',{reason,stack}};implicit-exception-cfg:incomplete"
    )
    val store = operator(term, Operators.assignment, Seq(reference(term, name, Some(region.slot)), normal), name)
    block(term, Seq(store, Ast(jumpTargetNode(term, region.end, region.end)), reference(term, name, Some(region.slot))))
  }

  /** Preserve the BIF call itself. For a nontrivial argument its subtree is evaluated exactly once, in argument 1; an
    * explicit lexical slot carries that reason to the enclosing catch result.
    */
  def capturedRaise(term: Term, scope: Scope, target: String, name: String): Option[Ast] =
    current.filter(_ => Set("erlang:throw/1", "erlang:exit/1", "erlang:error/1")(target)).map { region =>
      val arg                    = term(3).items.head
      val value                  = expr(arg, scope)
      val (argument, readReason) = if (arg.tag == "var") {
        value -> (() => reference(arg, arg(2).text, scope.lookup(arg(2).text)))
      } else {
        val tmp  = fresh("reason")
        val decl = local(arg, tmp, scope)
        val read = () => reference(arg, tmp, Some(decl))
        operator(arg, Operators.assignment, Seq(read(), value), code(arg)) -> read
      }
      val call = callAst(callNode(term, code(term), name, target, DispatchTypes.STATIC_DISPATCH), Seq(argument))
      def exitTuple(reason: Ast): Ast =
        operator(term, "<operator>.tuple", Seq(Ast(literalNode(term, "'EXIT'", "atom")), reason), "{'EXIT', Reason}")
      val caught = name match {
        case "throw" => readReason()
        case "exit"  => exitTuple(readReason())
        case _       =>
          val stack = operator(term, "<operator>.erlangStacktrace", Nil, "exception stack")
          exitTuple(operator(term, "<operator>.tuple", Seq(readReason(), stack), "{Reason, Stack}"))
      }
      val save =
        operator(term, Operators.assignment, Seq(reference(term, region.name, Some(region.slot)), caught), region.name)
      block(term, Seq(call, save, gotoAst(term, region.end, region.end)))
    }
}
