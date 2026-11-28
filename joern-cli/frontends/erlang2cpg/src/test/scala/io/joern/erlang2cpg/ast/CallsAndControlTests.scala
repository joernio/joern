package io.joern.erlang2cpg.ast

import io.joern.erlang2cpg.{Config, Erlang2Cpg}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, CfgNode, Method}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Files
import scala.collection.mutable
import scala.util.Using

class CallsAndControlTests extends AnyFunSuite {
  private def graph(sources: Seq[String])(check: Cpg => Unit): Unit =
    FileUtil.usingTemporaryDirectory("erl-control") { dir =>
      sources.zipWithIndex.foreach { case (source, i) => Files.writeString(dir.resolve(s"source$i.erl"), source) }
      Using.resource(new Erlang2Cpg().createCpgWithOverlays(Config().withInputPath(dir.toString)).get)(check)
    }

  private def only[A](nodes: IterableOnce[A]): A = {
    val results = nodes.iterator.toList
    assert(results.size == 1, results)
    results.head
  }

  private def reachable(start: CfgNode, end: CfgNode, avoid: Set[CfgNode] = Set.empty): Boolean = {
    val queue = mutable.Queue(start)
    val seen  = mutable.Set.empty[CfgNode]
    while (queue.nonEmpty) {
      val node = queue.dequeue()
      if (!avoid(node) && seen.add(node)) {
        if (node == end) return true
        queue ++= node._cfgOut.collect { case next: CfgNode => next }
      }
    }
    false
  }

  private def targets(site: Call): List[Method] = site._callOut.collect { case method: Method => method }.toList

  test("call edges follow module, arity, imports and visibility in either file order") {
    val caller =
      """-module(caller).
        |-import(provider, [pick/1]).
        |-compile({no_auto_import, [abs/1]}).
        |work(M,F,X) -> pick(X), provider:pick(X,X), provider:hidden(X), lists:reverse(X),
        |               is_atom(X), abs(X), local(X), M:F(X).
        |local(X) -> X.
        |ref() -> fun provider:pick/2.
        |""".stripMargin
    val provider =
      """-module(provider).
        |-export([pick/1,pick/2]).
        |pick(X) -> X.
        |pick(_,Y) -> Y.
        |hidden(X) -> X.
        |""".stripMargin

    for (sources <- Seq(Seq(caller, provider), Seq(provider, caller))) {
      graph(sources) { cpg =>
        val work                                 = only(cpg.method.fullNameExact("caller:work/3"))
        def site(name: String, arity: Int): Call = only(work.call.nameExact(name).filter(_.argument.size == arity))
        def assertEdge(name: String, arity: Int, targetName: String, external: Boolean): Unit = {
          val target = only(targets(site(name, arity)))
          assert(target.fullName == targetName && target.isExternal == external, target)
        }
        assertEdge("pick", 1, "provider:pick/1", external = false)
        assertEdge("pick", 2, "provider:pick/2", external = false)
        assertEdge("local", 1, "caller:local/1", external = false)
        assertEdge("reverse", 1, "lists:reverse/1", external = true)
        assertEdge("is_atom", 1, "erlang:is_atom/1", external = true)
        assertEdge("abs", 1, "caller:abs/1", external = true)
        val hidden = only(cpg.method.fullNameExact("provider:hidden/1"))
        assert(!targets(site("hidden", 1)).contains(hidden))
        val dynamic = only(work.call.codeExact("M:F(X)"))
        assert(dynamic.ast.isIdentifier.name.toSet == Set("M", "F", "X"))
        assert(targets(dynamic).forall(_.isExternal))
        val reference = only(cpg.method.fullNameExact("caller:ref/0"))
        assert(only(reference.ast.isMethodRef).methodFullName == "provider:pick/2")
        val namedTarget = only(cpg.method.fullNameExact("provider:pick/2"))
        assert(!reference.call.exists(call => targets(call).contains(namedTarget)))
      }
    }
  }

  test("ordered guards, short circuiting and try cleanup have executable CFG paths") {
    graph(Seq("""-module(branch).
        |choose({ok,X}) when is_atom(X) -> accepted(X);
        |choose(_) -> fallback().
        |shadow(_) -> first();
        |shadow(ok) -> unreachable().
        |mid(X) -> case X of _ -> first(); ok -> unreachable() end, done().
        |select(X) -> case X of first -> hit(); _ -> miss() end.
        |check(X) -> if is_atom(X) -> hit(); true -> miss() end.
        |both(X) -> one(X) andalso two(X).
        |either(X) -> one(X) orelse two(X).
        |clean(X) -> try work(X) of Y -> good(Y) catch _:Why -> bad(Why) after cleanup() end.
        |""".stripMargin)) { cpg =>
      def method(name: String): Method            = only(cpg.method.fullNameExact(s"branch:$name/1"))
      def call(owner: Method, name: String): Call = only(owner.call.nameExact(name))
      val choose                                  = method("choose")
      val success                                 = call(choose, "accepted")
      val failure                                 = call(choose, "fallback")
      val guard                                   = call(choose, "is_atom")
      assert(reachable(guard, success, Set(failure)) && reachable(guard, failure, Set(success)))
      assert(reachable(success, choose.methodReturn) && !reachable(success, failure))
      for (name <- Seq("shadow", "mid")) {
        val owner = method(name)
        val entry = call(owner, "first")
        val dead  = call(owner, "unreachable")
        assert(reachable(owner, entry) && !reachable(owner, dead))
        assert(reachable(entry, owner.methodReturn))
      }
      for (name <- Seq("select", "check")) {
        val owner = method(name)
        val hit   = call(owner, "hit")
        val miss  = call(owner, "miss")
        assert(reachable(owner, hit, Set(miss)) && reachable(owner, miss, Set(hit)))
        assert(!reachable(hit, miss))
      }
      for (name <- Seq("both", "either")) {
        val owner = method(name)
        val one   = call(owner, "one")
        val two   = call(owner, "two")
        assert(reachable(one, two) && reachable(one, owner.methodReturn, Set(two)))
      }
      val clean   = method("clean")
      val cleanup = call(clean, "cleanup")
      val work    = call(clean, "work")
      val good    = call(clean, "good")
      val bad     = call(clean, "bad")
      assert(reachable(work, good, Set(bad)) && reachable(work, bad, Set(good)))
      assert(!reachable(good, bad) && !reachable(bad, good))
      for (site <- Seq(good, bad)) {
        assert(reachable(site, cleanup) && reachable(cleanup, clean.methodReturn))
        assert(!reachable(site, clean.methodReturn, Set(cleanup)))
      }
    }
  }
}
