package io.joern.erlang2cpg.ast

import io.joern.erlang2cpg.parser.Term
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.Operators

private[ast] trait ErlangProcesses { this: ModuleAst =>

  def process(term: Term, scope: Scope, tail: Boolean): Option[Ast] = term.tag match {
    case "op" if term(2) == Term.Atom("!") =>
      val send = operator(term, "<operator>.erlangSend", Seq(expr(term(3), scope), expr(term(4), scope)), code(term))
      Some(if (tail) returned(term, send) else send)
    case "receive" => Some(receive(term, scope, tail))
    case _         => None
  }

  /** The mailbox value is local to this receive. A timed receive can take its timeout branch even with a catch-all
    * head, when the mailbox contains no message. Evaluate the timeout expression once before selecting either path.
    */
  private def receive(term: Term, scope: Scope, tail: Boolean): Ast = {
    val rows               = term(2).items
    val timeout            = term.items.lift(3).map(expr(_, scope))
    def timeoutBody(): Ast = block(term, statements(term(4).items, scope.child(), tail))
    if (rows.isEmpty) return block(term, timeout.toSeq :+ timeoutBody())

    val result                  = Option.unless(tail)(local(term, fresh("receiveResult"), scope))
    def finish(value: Ast): Ast = result match {
      case None              => value
      case Some(declaration) =>
        val dest = reference(term, declaration.name, Some(declaration))
        val save = operator(term, Operators.assignment, Seq(dest, value), declaration.name)
        block(term, Seq(save, breakAst(term, "end receive")))
    }

    val message      = local(term, fresh("message"), scope)
    def messageRef() = reference(term, message.name, Some(message))
    val value        = operator(term, "<operator>.erlangReceive", Nil, "receive")
    val assign       = operator(term, Operators.assignment, Seq(messageRef(), value), s"${message.name} = receive")
    val retry        = continueAst(term, "next message")
    val selection    = clauses(rows, Seq(() => messageRef()), scope, tail, Some(retry))
    val read         = block(term, Seq(assign, finish(selection)))

    val scan = timeout match {
      case Some(_) =>
        val ready = operator(term, "<operator>.erlangReceiveReady", Nil, "message before timeout")
        ifThenElseAst(term, Some(ready), read, Some(finish(timeoutBody())), Some("receive"))
      case None => read
    }
    val loop = forAst(term, Nil, timeout.toSeq, Nil, Nil, Seq(scan), Some("receive"))
    result.fold(loop)(decl => block(term, Seq(loop, reference(term, decl.name, Some(decl)))))
  }

  /** OTP callback specifications describe a function type. The implementation, when present, retains its own METHOD. */
  def callback(attribute: Term, filename: String): Option[Ast] = {
    Option.when(attribute.tag == "attribute" && attribute(2) == Term.Atom("callback")) {
      val spec        = attribute(3)
      val name        = s"${spec(0)(0).text}/${spec(0)(1).text}"
      val declaration = typeDeclNode(
        attribute,
        name,
        s"$module:<callback>:$name",
        filename,
        s"-callback $name",
        genericSignature = Some(spec(1).text)
      )
      Ast(declaration)
    }
  }
}
