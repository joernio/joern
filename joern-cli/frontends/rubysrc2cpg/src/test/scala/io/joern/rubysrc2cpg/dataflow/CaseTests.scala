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

  // TODO:
  "Data flow through a when argument context" ignore {
    val cpg = code("""
                         |x = 10
                         |
                         |case x
                         |
                         |when 1..5
                         |    y = x
                         |when 5..10
                         |    z = x
                         |when 10..15
                         |    w = x
                         |else
                         |    _p = x
                         |end
                         |
                         |puts _p
                         |puts w
                         |puts y
                         |puts z
                         |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 3
  }
}
