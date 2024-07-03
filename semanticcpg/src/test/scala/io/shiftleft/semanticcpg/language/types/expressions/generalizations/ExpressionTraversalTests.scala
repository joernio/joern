package io.shiftleft.semanticcpg.language.types.expressions.generalizations

import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.testing.MockCpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ExpressionTraversalTests extends AnyWordSpec with Matchers {

  val cpg = MockCpg()
    .withMethod("methodForCfgTest")
    .withCallInMethod("methodForCfgTest", "call1")
    .withCallInMethod("methodForCfgTest", "call2")
    .withCustom { (graph, cpg) =>
      val method = cpg.method("methodForCfgTest").head
      val call1  = cpg.call.name("call1").head
      val call2  = cpg.call.name("call2").head
      graph.addEdge(method, call1, EdgeTypes.CFG)
      graph.addEdge(call1, call2, EdgeTypes.CFG)
      graph.addEdge(call2, method.methodReturn, EdgeTypes.CFG)
    }
    .cpg

  "generic cpg" should {
    "expand to next expression in CFG" in {
      val List(x) = cpg.method.name("methodForCfgTest").cfgFirst.cfgNext.isCall.l
      x.name shouldBe "call2"
    }

    "don't expand to previous of first expression in CFG. Aka it should be empty" in {
      cpg.method.name("methodForCfgTest").cfgFirst.cfgPrev.isExpression.size shouldBe 0
    }

    "expand to previous expression in CFG" in {
      val List(x) = cpg.method.name("methodForCfgTest").cfgLast.cfgPrev.isCall.l
      x.name shouldBe "call1"
    }

    "don't expand to next of last expression in CFG. Aka it should be empty" in {
      cpg.method.name("methodForCfgTest").cfgLast.cfgNext.isExpression.size shouldBe 0
    }
  }
}
