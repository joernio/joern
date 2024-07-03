package io.shiftleft.semanticcpg.language.types.expressions.generalizations

import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.testing.MockCpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CfgNodeTraversalTests extends AnyWordSpec with Matchers {

  implicit val resolver: ICallResolver = NoResolve

  val cpg = MockCpg()
    .withMethod("methodForCfgTest")
    .withCallInMethod("methodForCfgTest", "call1")
    .withCallInMethod("methodForCfgTest", "call2")
    .withCallInMethod("methodForCfgTest", "call3")
    .withCustom { (graph, cpg) =>
      val method = cpg.method("methodForCfgTest").head
      val call1  = cpg.call.name("call1").head
      val call2  = cpg.call.name("call2").head
      val call3  = cpg.call.name("call3").head
      graph.addEdge(method, call1, EdgeTypes.CFG)
      graph.addEdge(call1, call2, EdgeTypes.CFG)
      graph.addEdge(call2, call3, EdgeTypes.CFG)
      graph.addEdge(call2, method.methodReturn, EdgeTypes.CFG)
    }
    .cpg

  "should allow traversing to CFG predecessors recursively" in {
    cpg.method.call.name("call3").cfgPrev(2).isCall.name.l shouldBe List("call2", "call1")
  }

  "should allow traversing to CFG successors recursively" in {
    cpg.method.call.name("call1").cfgNext(2).isCall.name.l shouldBe List("call2", "call3")
  }

}
