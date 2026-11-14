package io.joern.c2cpg.dataflow

import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.queryengine.condition.CallRule
import io.shiftleft.semanticcpg.language.*

class ConditionalCallAnalyzerTests extends DataFlowCodeToCpgSuite {

  "OpenBSD M_WAITOK during suspend" should {
    val cpg = code("""
        |#define M_WAITOK 1
        |#define DVACT_SUSPEND 2
        |void panic();
        |void* malloc(int size, int type, int flags);
        |
        |void test_activate(int action) {
        |    if (action == DVACT_SUSPEND) {
        |        malloc(1024, 0, M_WAITOK); // target bad call
        |    } else {
        |        panic();                   // hard stop
        |        malloc(128, 0, M_WAITOK);  // unreachable
        |    }
        |}
        |""".stripMargin)

    val method       = cpg.method("test_activate").head
    val action       = method.parameter.name("action").head
    val mallocWaitOk = CallRule("malloc", argument = Some((3, BigInt(1))), expanded = true)
    val panic        = CallRule("panic")
    val targetCalls  = Vector(mallocWaitOk)
    val hardStops    = Vector(panic)

    "find the sleeping malloc when action is DVACT_SUSPEND" in {
      val results =
        method.conditionallyReachesCalls(Map(action -> BigInt(2)), targetCalls, hardStops).l
      results.size shouldBe 1
      val result = results.head
      result.root shouldBe method
      result.target.name shouldBe "malloc"
      result.target.code shouldBe "malloc(1024, 0, M_WAITOK)"
    }

    "find no sleeping malloc when action is not DVACT_SUSPEND (pruned at panic)" in {
      val results =
        method.conditionallyReachesCalls(Map(action -> BigInt(0)), targetCalls, hardStops).l
      results shouldBe empty
    }
  }

}
