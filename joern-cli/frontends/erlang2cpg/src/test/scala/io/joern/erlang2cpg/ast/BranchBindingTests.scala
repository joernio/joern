package io.joern.erlang2cpg.ast

import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.erlang2cpg.{Config, Erlang2Cpg}
import io.shiftleft.codepropertygraph.generated.nodes.CfgNode
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Files
import scala.collection.mutable
import scala.util.Using

class BranchBindingTests extends AnyFunSuite {
  private implicit val context: EngineContext = EngineContext(DefaultSemantics())

  private def one[A](values: Iterator[A]): A = {
    val nodes = values.toList
    assert(nodes.size == 1, nodes)
    nodes.head
  }

  private def reaches(start: CfgNode, end: CfgNode, avoiding: Set[CfgNode] = Set.empty): Boolean = {
    val todo = mutable.Queue(start)
    val seen = mutable.Set.empty[CfgNode]
    while (todo.nonEmpty) {
      val node = todo.dequeue()
      if (!avoiding(node) && seen.add(node)) {
        if (node == end) return true
        todo ++= node.cfgNext
      }
    }
    false
  }

  test("case exports a shared declaration and only normal clause exits reach its subsequent use") {
    FileUtil.usingTemporaryDirectory("erlang-branch-bindings") { root =>
      val source = """-module(joined).
                     |pick(Input, A, B) ->
                     |    case Input of
                     |        {ok,N} when is_integer(N) -> X = A;
                     |        {ok,_} -> X = B
                     |    end,
                     |    observe(X).
                     |observe(Value) -> Value.
                     |""".stripMargin
      Files.writeString(root.resolve("joined.erl"), source)
      Using.resource(new Erlang2Cpg().createCpgWithOverlays(Config().withInputPath(root.toString)).get) { cpg =>
        new OssDataFlow(new OssDataFlowOptions()).run(new LayerCreatorContext(cpg))
        val method = one(cpg.method.fullNameExact("joined:pick/3"))
        val decl   = one(method.ast.isLocal.nameExact("X"))
        val uses   = method.ast.isIdentifier.nameExact("X").l
        assert(uses.map(_.lineNumber).toSet == Set(Some(4), Some(5), Some(7)))
        assert(uses.forall(_.refsTo.l == List(decl)))
        val sink = one(method.call.nameExact("observe").argument(1))
        for (name <- Seq("A", "B")) {
          val parameter = one(method.parameter.nameExact(name))
          assert(Iterator(sink).reachableBy(Iterator(parameter)).nonEmpty, name)
        }
        val guard   = one(method.call.nameExact("is_integer"))
        val first   = one(method.call.codeExact("X = A"))
        val second  = one(method.call.codeExact("X = B"))
        val failure = one(method.controlStructure.codeExact("no matching clause"))
        assert(reaches(guard, first, Set(second)))
        assert(reaches(guard, second, Set(first)))
        assert(!reaches(failure, sink))
      }
    }
  }
}
