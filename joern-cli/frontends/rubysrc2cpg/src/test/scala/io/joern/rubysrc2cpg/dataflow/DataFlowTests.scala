package io.joern.rubysrc2cpg.dataflow

import io.joern.rubysrc2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.joern.dataflowengineoss.language._
import io.shiftleft.semanticcpg.language._

class DataFlowTests extends DataFlowCodeToCpgSuite {

  "CPG for code with flow through a function and if-elseif-else" should {
    val cpg = code("""
        |x = 2
        |a = x
        |b = 0
        |
        |if a > 2
        |    b = a + 3
        |elseif a > 4
        |    b = a + 5
        |elseif a > 8
        |    b = a + 5
        |else
        |    b = a + 9
        |end
        |
        |puts(b)
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).l.size shouldBe 2
    }
  }
}
