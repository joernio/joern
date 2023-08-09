package io.joern.rubysrc2cpg.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.rubysrc2cpg.RubySrc2Cpg
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class DataFlowTests extends RubyCode2CpgFixture(withPostProcessing = true, withDataFlow = true) {

  "Data flow through packing left hand side with others" should {
    val cpg = code("""
          |x = 1
          |y = 2
          |z = 3
          |p,*a = z,y,x
          |puts a
          |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

}
