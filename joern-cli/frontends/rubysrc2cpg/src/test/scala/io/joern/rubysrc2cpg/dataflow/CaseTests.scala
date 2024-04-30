package io.joern.rubysrc2cpg.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class CaseTests extends RubyCode2CpgFixture(withPostProcessing = true, withDataFlow = true) {
  "Data flow through case statement" in {
    val cpg = code("""
                     |x = 2
                     |b = x
                     |
                     |case b
                     |when 1
                     |    puts b
                     |when 2
                     |    puts b
                     |when 3
                     |    puts b
                     |else
                     |    puts b
                     |end
                     |
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 8
  }
}
