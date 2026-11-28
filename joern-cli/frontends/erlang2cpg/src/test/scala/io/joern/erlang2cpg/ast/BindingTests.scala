package io.joern.erlang2cpg.ast

import io.joern.erlang2cpg.{Config, Erlang2Cpg}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Files
import scala.util.Using

class BindingTests extends AnyFunSuite {
  private def withGraph(body: String)(check: Cpg => Unit): Unit = {
    FileUtil.usingTemporaryDirectory("erlang-bindings") { dir =>
      val input  = Files.writeString(dir.resolve("binding.erl"), "-module(binding).\n" + body)
      val config = Config().withInputPath(input.toString).withOutputPath(dir.resolve("out.cpg").toString)
      Using.resource(new Erlang2Cpg().createCpg(config).get)(check)
    }
  }

  test("a repeated name shares one declaration, anonymous underscores have no declaration") {
    withGraph("f({X,X},{_,_},{_Name,_Name}) -> {X,_Name}.") { cpg =>
      for (name <- Seq("X", "_Name")) {
        val decls = cpg.local.nameExact(name).l
        assert(decls.size == 1)
        val uses = cpg.identifier.nameExact(name).l
        assert(uses.size == 3)
        assert(uses.forall(_.refsTo.l == decls))
      }
      assert(cpg.identifier.nameExact("_").size == 2)
      assert(cpg.identifier.nameExact("_").refsTo.isEmpty)
    }
  }

  test("a later binary segment size refers to the local bound by the first segment") {
    withGraph("f(<<N:8, Rest:N/binary>>) -> Rest.") { cpg =>
      val declaration = cpg.local.nameExact("N").l
      val uses        = cpg.identifier.nameExact("N").l
      assert(declaration.size == 1)
      assert(uses.size == 2)
      assert(uses.forall(_.refsTo.l == declaration))
      assert(uses.map(_.columnNumber).toSet == Set(Some(5), Some(15)))
    }
  }

  test("fun parameters shadow outer bindings and free variables have explicit closure bindings") {
    withGraph("f(X,Y) -> fun(X) -> {X,Y} end.") { cpg =>
      val outer  = cpg.method.fullNameExact("binding:f/2").head
      val inner  = cpg.method.isExternal(false).filter(_.fullName != outer.fullName).head
      val outerX = outer.parameter.nameExact("X").head
      val innerX = inner.parameter.nameExact("X").head
      assert(innerX != outerX)
      assert(inner.ast.isIdentifier.nameExact("X").nonEmpty)
      assert(inner.ast.isIdentifier.nameExact("X").forall(_.refsTo.l == List(innerX)))
      val copy     = inner.ast.isLocal.nameExact("Y").head
      val bindings = cpg.closureBinding.l
      assert(bindings.size == 1)
      assert(bindings.head.closureBindingId == copy.closureBindingId)
      assert(bindings.head._refOut.toList == outer.parameter.nameExact("Y").l)
      assert(outer.ast.isMethodRef.head._captureOut.toList == bindings)
    }
  }

  test("map keys are reads, record fields bind payloads and list tails retain their declaration") {
    val source = """-record(item, {value :: term()}).
                   |f(K, M) ->
                   |  #{K := #item{value = [H|T]}} = M,
                   |  {H,T}.
                   |""".stripMargin
    withGraph(source) { cpg =>
      assert(cpg.typeDecl.fullNameExact("binding:#item").member.name.l == List("value"))
      val key = cpg.method.nameExact("f").parameter.nameExact("K").head
      assert(cpg.identifier.nameExact("K").size >= 2)
      assert(cpg.identifier.nameExact("K").forall(_.refsTo.l == List(key)))
      for (name <- Seq("H", "T")) {
        val local = cpg.local.nameExact(name).head
        assert(cpg.identifier.nameExact(name).size == 2)
        assert(cpg.identifier.nameExact(name).forall(_.refsTo.l == List(local)))
      }
    }
  }

  test("nonexecutable forms produce no calls and unsupported expressions retain their location and kind") {
    withGraph("-spec f() -> [atom()].\nf() -> [X || X <- []].") { cpg =>
      assert(cpg.method.fullNameExact("binding:f/0").size == 1)
      val unsupported = cpg.unknown.parserTypeNameExact("lc").l
      assert(unsupported.map(_.lineNumber) == List(Some(3)))
      assert(cpg.call.isEmpty)
    }
  }
}
