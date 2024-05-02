package io.joern.rubysrc2cpg.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class RangeTests extends RubyCode2CpgFixture(withPostProcessing = true, withDataFlow = true) {
  // Works in deprecated
  "Data flows through range operators" ignore {
    val cpg = code("""
                     |x = 10
                     |y=0
                     |for i in 1...10 do
                     |   x += i
                     |   if (x > 10)
                     |     y = x
                     |   end
                     |end
                     |
                     |puts y
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 3
  }
}
