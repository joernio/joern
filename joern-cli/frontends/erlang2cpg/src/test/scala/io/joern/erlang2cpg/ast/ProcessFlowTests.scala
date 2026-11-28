package io.joern.erlang2cpg.ast

import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.erlang2cpg.{Config, Erlang2Cpg}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Files
import scala.util.Using

class ProcessFlowTests extends AnyFunSuite {
  private implicit val context: EngineContext = EngineContext(DefaultSemantics())

  private def withGraph(test: Cpg => Unit): Unit = FileUtil.usingTemporaryDirectory("process-flow") { root =>
    Files.writeString(
      root.resolve("worker.erl"),
      """-module(worker).
        |-export([start/1, signal/2, read/0, through/2, handle_call/3]).
        |-callback handle_call(term(), term(), term()) -> term().
        |start(Value) -> Pid = spawn(fun() -> helper:observe(Value) end), helper:pid(Pid).
        |signal(Pid, Value) -> Sent = Pid ! Value, helper:observe(Sent).
        |read() -> receive {ok, Payload} -> helper:observe(Payload) after 0 -> idle end.
        |through(A, B) -> Chosen = helper:keep(A, B), helper:observe(Chosen).
        |handle_call(Request, _From, _State) -> Request.
        |""".stripMargin
    )
    Files.writeString(
      root.resolve("helper.erl"),
      """-module(helper).
        |-export([keep/2, observe/1, pid/1]).
        |keep(_Ignored, Value) -> Value.
        |observe(Value) -> Value.
        |pid(Pid) -> Pid.
        |""".stripMargin
    )
    Using.resource(new Erlang2Cpg().createCpgWithOverlays(Config().withInputPath(root.toString)).get) { cpg =>
      new OssDataFlow(new OssDataFlowOptions()).run(new LayerCreatorContext(cpg))
      test(cpg)
    }
  }

  private def one[T](results: IterableOnce[T]): T = {
    val values = results.iterator.toList
    assert(values.size == 1, s"expected one endpoint, found $values")
    values.head
  }

  test("capture, spawn result, local send result, receive binding and callback declaration") {
    withGraph { cpg =>
      val captured  = one(cpg.method.fullNameExact("worker:start/1").parameter.index(1))
      val insideFun = one(cpg.call.methodFullNameExact("helper:observe/1").lineNumber(4).argument(1))
      assert(Iterator(insideFun).reachableBy(Iterator(captured)).nonEmpty)
      val spawned   = one(cpg.method.fullNameExact("worker:start/1").call.nameExact("pid").argument(1))
      val spawnCall = one(cpg.call.methodFullNameExact("erlang:spawn/1"))
      assert(spawnCall.argument(1).isMethodRef)
      assert(Iterator(spawned).reachableBy(Iterator(captured)).isEmpty)

      val signal = one(cpg.method.fullNameExact("worker:signal/2"))
      val send   = one(signal.call.methodFullNameExact("<operator>.erlangSend"))
      assert(send.argument(1).code == "Pid" && send.argument(2).code == "Value")
      val signalResult = one(signal.call.nameExact("observe").argument(1))
      val pid          = one(signal.parameter.index(1))
      val message      = one(signal.parameter.index(2))
      assert(Iterator(signalResult).reachableBy(Iterator(message)).nonEmpty)
      assert(Iterator(signalResult).reachableBy(Iterator(pid)).isEmpty)

      val read    = one(cpg.method.fullNameExact("worker:read/0"))
      val sink    = one(read.call.methodFullNameExact("helper:observe/1").argument(1))
      val matched = one(read.ast.isIdentifier.nameExact("Payload").filter(_.id != sink.id))
      assert(Iterator(sink).reachableBy(Iterator(matched)).nonEmpty)
      assert(Iterator(sink).reachableBy(Iterator(message)).isEmpty)

      val callback = one(cpg.typeDecl.fullNameExact("worker:<callback>:handle_call/3"))
      assert(callback.lineNumber.contains(3))
      val definition = one(cpg.method.fullNameExact("worker:handle_call/3"))
      assert(!definition.isExternal && definition.parameter.size == 3)
    }
  }

  test("the selected parameter reaches the observer through the cross-module return") {
    withGraph { cpg =>
      val through = one(cpg.method.fullNameExact("worker:through/2"))
      val keep    = one(through.call.methodFullNameExact("helper:keep/2"))
      val target  = one(keep._callOut.collect { case method: Method => method })
      assert(target.fullName == "helper:keep/2")
      val sink   = one(through.call.methodFullNameExact("helper:observe/1").argument(1))
      val first  = one(through.parameter.index(1))
      val second = one(through.parameter.index(2))
      assert(Iterator(sink).reachableBy(Iterator(first)).isEmpty)
      val paths = Iterator(sink).reachableByFlows(Iterator(second)).toList
      assert(paths.nonEmpty)
      info(paths.head.elements.map(node => s"${node.lineNumber}: ${node.code}").mkString(" -> "))
    }
  }
}
