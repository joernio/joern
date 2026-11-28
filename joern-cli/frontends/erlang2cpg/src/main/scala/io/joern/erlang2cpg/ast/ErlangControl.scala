package io.joern.erlang2cpg.ast

import io.joern.erlang2cpg.parser.Term
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.NewLocal

import scala.collection.mutable

private[ast] trait ErlangControl { this: ModuleAst =>
  private def logical(term: Term, name: String, operands: Seq[Ast]): Option[Ast] =
    operands.reduceLeftOption((left, right) => operator(term, name, Seq(left, right), code(term)))

  private def guard(term: Term, scope: Scope): Option[Ast] = {
    val alternatives = term(3).items.flatMap { sequence =>
      logical(term, Operators.logicalAnd, sequence.items.map(expr(_, scope)))
    }
    logical(term, Operators.logicalOr, alternatives)
  }

  /** A clause either succeeds as a whole or transfers control to the next clause. Head bindings are in scope for its
    * guards and body only. A failed final clause raises a match exception.
    */
  def clauses(
    rows: Seq[Term],
    inputs: Seq[() => Ast],
    scope: Scope,
    tail: Boolean,
    fallback: Option[Ast] = None,
    exportBindings: Boolean = false
  ): Ast = {
    val shared =
      Option.when(exportBindings)(scope.alternativeLocals.getOrElse(mutable.LinkedHashMap.empty[String, NewLocal]))
    val exits  = mutable.ArrayBuffer.empty[Scope]
    val result = Option.unless(tail) {
      val name = fresh("clauseResult")
      name -> local(rows.head, name, scope)
    }
    def finish(term: Term, value: Ast): Ast = result match {
      case None                      => value
      case Some((name, declaration)) =>
        operator(term, Operators.assignment, Seq(reference(term, name, Some(declaration)), value), name)
    }

    def select(rowsLeft: List[Term]): Ast = {
      val row      = rowsLeft.head
      val patterns = row(2).items
      val branch   = scope.forHead(patterns)
      branch.alternativeLocals = shared
      if (exportBindings) exits += branch
      val pairs       = patterns.zip(inputs)
      val before      = pairs.flatMap { case (pattern, input) => bind(pattern, input, branch) }
      val headCanFail = patterns.exists(_.tag != "var") || before.exists(_.root.exists {
        case call: io.shiftleft.codepropertygraph.generated.nodes.NewCall => call.name == Operators.equals
        case _                                                            => false
      })
      val matched =
        if (headCanFail) Some(operator(row, "<operator>.patternMatches", before, patterns.map(code).mkString(", ")))
        else None
      val condition   = logical(row, Operators.logicalAnd, matched.toSeq ++ guard(row, branch))
      val expressions = row(4).items
      val last        = expressions.last
      val body        =
        block(row, statements(expressions.init, branch, tail = false) :+ finish(last, expr(last, branch, tail)))
      val decision = condition match {
        case None if rowsLeft.tail.nonEmpty =>
          // Keep shadowed clauses queryable while the irrefutable clause jumps past them in the CFG.
          val shadowed = select(rowsLeft.tail)
          if (tail) block(row, Seq(body, shadowed))
          else {
            val done = fresh("clauseEnd")
            block(row, Seq(body, gotoAst(row, done, done), shadowed, Ast(jumpTargetNode(row, done, done))))
          }
        case None       => body
        case Some(test) =>
          val next = rowsLeft.tail match {
            case Nil  => fallback.map(finish(row, _)).getOrElse(throwAst(row, Nil, Some("no matching clause")))
            case rest => select(rest)
          }
          ifThenElseAst(row, Some(test), body, Some(next), Some("clause"))
      }
      if (matched.isEmpty && before.nonEmpty) block(row, before :+ decision) else decision
    }
    val selection = select(rows.toList)
    shared.foreach { locals =>
      locals.foreach { case (name, declaration) =>
        if (exits.forall(_.bindings.get(name).contains(declaration))) scope.bindings(name) = declaration
      }
    }
    result match {
      case Some((name, declaration)) => block(rows.head, Seq(selection, reference(rows.head, name, Some(declaration))))
      case None                      => selection
    }
  }

  private def stored(term: Term, scope: Scope, label: String, value: Ast): (Ast, () => Ast) = {
    val name        = fresh(label)
    val declaration = local(term, name, scope)
    val read        = () => reference(term, name, Some(declaration))
    operator(term, Operators.assignment, Seq(read(), value), name) -> read
  }

  private def switchOn(term: Term, rows: Seq[Term], value: Ast, scope: Scope, tail: Boolean): Ast = {
    val (assign, read) = stored(term, scope, "subject", value)
    block(term, Seq(assign, clauses(rows, Seq(read), scope, tail, exportBindings = true)))
  }

  private def tryExpression(term: Term, scope: Scope, tail: Boolean): Ast = {
    val protectedBody    = term(2).items
    val ofClauses        = term(3).items
    val catchClauses     = term(4).items
    val cleanup          = term(5).items
    val result           = fresh("tryResult")
    val resultDecl       = local(term, result, scope)
    def resultRef        = reference(term, result, Some(resultDecl))
    def save(value: Ast) = operator(term, Operators.assignment, Seq(resultRef, value), result)

    val branch  = scope.child()
    val tryBody =
      block(term, statements(protectedBody.init, branch, tail = false) :+ save(expr(protectedBody.last, branch)))
    // OTP selects an of clause only after the protected expressions complete normally. Materialize
    // the exception outcome before clause selection so a handler need never execute an of body.
    val exception          = fresh("exception")
    val caught             = local(term, exception, scope)
    def exceptionRef       = reference(term, exception, Some(caught))
    def record(value: Ast) = operator(term, Operators.assignment, Seq(exceptionRef, value), exception)
    def nilValue           = Ast(literalNode(term, "[]", "nil"))
    def completed          = operator(term, Operators.equals, Seq(exceptionRef, nilValue), "completed normally")
    val init               = record(nilValue)
    val capture            = record(operator(term, "<operator>.caughtException", Nil, "caught exception"))
    val handler            =
      Ast(controlStructureNode(term, ControlStructureTypes.CATCH, "catch")).withChild(block(term, Seq(capture)))
    val attempted = tryCatchAst(term, tryBody, Seq(handler), None, Some("try"))

    val success =
      if (ofClauses.isEmpty) block(term, Nil)
      else {
        val noMatch = record(operator(term, "<operator>.tryClauseError", Seq(resultRef), "try_clause"))
        block(term, Seq(save(clauses(ofClauses, Seq(() => resultRef), branch, tail = false, Some(noMatch)))))
      }
    val failed = Option.when(catchClauses.nonEmpty) {
      val (preserve, exceptionValue) = stored(term, scope, "caught", exceptionRef)
      val caughtValue = clauses(catchClauses, Seq(exceptionValue), scope, tail = false, Some(record(exceptionValue())))
      block(term, Seq(preserve, record(nilValue), save(caughtValue)))
    }
    val completion = ifThenElseAst(term, Some(completed), success, failed, Some("try outcome"))
    val after      = Option.when(cleanup.nonEmpty) {
      Ast(controlStructureNode(term, ControlStructureTypes.FINALLY, "after"))
        .withChild(block(term, statements(cleanup, scope.child(), tail = false)))
    }
    val raise =
      ifThenElseAst(term, Some(completed), block(term, Nil), Some(throwAst(term, Seq(exceptionRef), Some("reraise"))))
    val value = if (tail) returned(term, resultRef) else resultRef
    block(term, Seq(init, attempted, completion) ++ after ++ Seq(raise, value))
  }

  def control(term: Term, scope: Scope, tail: Boolean): Option[Ast] = term.tag match {
    case "case" => Some(switchOn(term, term(3).items, expr(term(2), scope), scope, tail))
    case "if"   => Some(clauses(term(2).items, Nil, scope, tail, exportBindings = true))
    case "try"  => Some(tryExpression(term, scope, tail))
    case "op" if Set("andalso", "orelse")(term(2).text) =>
      val name  = if (term(2).text == "andalso") Operators.logicalAnd else Operators.logicalOr
      val value = operator(term, name, Seq(expr(term(3), scope), expr(term(4), scope)), code(term))
      Some(if (tail) returned(term, value) else value)
    case _ => None
  }
}
