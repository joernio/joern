package io.joern.erlang2cpg.ast

import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.erlang2cpg.{Config, Erlang2Cpg}
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNode, Method}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Files
import scala.collection.mutable
import scala.util.Using

class CatchExpressionTests extends AnyFunSuite {
  private implicit val context: EngineContext = EngineContext(DefaultSemantics())

  private def reaches(start: CfgNode, end: CfgNode): Boolean = {
    val todo = mutable.Queue(start)
    val seen = mutable.Set.empty[CfgNode]
    while (todo.nonEmpty) {
      val node = todo.dequeue()
      if (seen.add(node)) {
        if (node == end) return true
        todo ++= node.cfgNext
      }
    }
    false
  }

  test("catch retains executable operands and transfers explicit raises past the unreachable suffix") {
    val source = """-module(captured).
                   |-export([normal/2, thrown/2, side/1]).
                   |normal(A, U) -> Chosen = catch side(captured:side(A)), observe(Chosen).
                   |thrown(A, Dead) -> Chosen = catch begin throw(A), side(Dead) end, observe(Chosen).
                   |exit_case(A) -> catch erlang:exit(A).
                   |error_case(A) -> catch erlang:error(A).
                   |nested(A) -> catch (catch throw(A)).
                   |side(A) -> A.
                   |observe(A) -> A.
                   |""".stripMargin
    FileUtil.usingTemporaryDirectory("erlang-catch-test") { root =>
      Files.writeString(root.resolve("captured.erl"), source)
      Using.resource(new Erlang2Cpg().createCpgWithOverlays(Config().withInputPath(root.toString)).get) { cpg =>
        new OssDataFlow(new OssDataFlowOptions()).run(new LayerCreatorContext(cpg))
        def one[A](nodes: Iterator[A]): A = {
          val rows = nodes.toList
          assert(rows.size == 1, rows)
          rows.head
        }
        val normal = one(cpg.method.fullNameExact("captured:normal/2").filterNot(_.isExternal))
        val caught = one(normal.call.nameExact("<operator>.erlangCatch"))
        assert(caught.tag.nameExact("erlang.catch.v1").size == 1)
        val calls = caught.ast.isCall.nameExact("side").l
        assert(calls.size == 2 && calls.forall(_._callOut.collect { case method: Method =>
          method.fullName
        }.toList == List("captured:side/1")))
        assert(calls.map(_.columnNumber).distinct.size == 2 && calls.forall(_.lineNumber.contains(3)))
        val selected = one(normal.parameter.nameExact("A"))
        val unused   = one(normal.parameter.nameExact("U"))
        assert(calls.flatMap(_.argument.isIdentifier.nameExact("A").refsTo.l) == List(selected))
        val sink = one(normal.call.nameExact("observe").argument(1))
        assert(cpg.metaData.head.overlays.contains("dataflowOss") && normal.ast.forall(_.label != "UNKNOWN"))
        assert(Iterator(sink).reachableBy(Iterator(selected)).nonEmpty)
        assert(Iterator(sink).reachableBy(Iterator(unused)).isEmpty)

        val thrown = one(cpg.method.fullNameExact("captured:thrown/2"))
        val raises = one(thrown.call.nameExact("throw"))
        val dead   = one(thrown.call.nameExact("side"))
        val next   = one(thrown.call.nameExact("observe"))
        assert(reaches(thrown, raises) && !reaches(raises, dead) && reaches(raises, next))
        val thrownSink = one(Iterator(next).argument(1))
        assert(Iterator(thrownSink).reachableBy(Iterator(one(thrown.parameter.nameExact("A")))).nonEmpty)
        assert(Iterator(thrownSink).reachableBy(Iterator(one(thrown.parameter.nameExact("Dead")))).isEmpty)
        val exited  = one(cpg.method.fullNameExact("captured:exit_case/1"))
        val errored = one(cpg.method.fullNameExact("captured:error_case/1"))
        assert(exited.call.nameExact("<operator>.tuple").size == 1)
        assert(errored.call.nameExact("<operator>.tuple").size == 2)
        assert(errored.call.nameExact("<operator>.erlangStacktrace").size == 1)
        assert(one(cpg.method.fullNameExact("captured:nested/1")).call.nameExact("<operator>.erlangCatch").size == 2)
      }
    }
  }
}
