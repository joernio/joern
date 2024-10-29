package io.joern.rubysrc2cpg.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class ConditionalTests extends RubyCode2CpgFixture(withPostProcessing = true, withDataFlow = true) {

  "flow through both branches of a ternary `.. ? .. : ..` operator" in {
    val cpg = code("""
        |x = 1
        |y = 2
        |z = foo ? x : y
        |puts z
        |""".stripMargin)
    val source = cpg.literal
    val sink   = cpg.method.name("puts").callIn.argument
    val flows  = sink.reachableByFlows(source)
    flows.map(flowToResultPairs).sortBy(_.headOption.map(_._1)).toSet shouldBe
      Set(List(("x = 1", 2), ("z = x", 4), ("puts z", 5)), List(("y = 2", 3), ("z = y", 4), ("puts z", 5)))
  }

  "flow through statement with ternary operator with multiple line" in {
    val cpg = code("""
                     |x = 2
                     |y = 3
                     |z = 4
                     |
                     |w = x == 2 ?
                     | y
                     | : z
                     |puts y
                     |""".stripMargin)

    val source = cpg.literal.code("3").l
    val sink   = cpg.call.name("puts").argument(1).l
    sink.reachableByFlows(source).size shouldBe 1
  }

  "flow through method conditional over-approximates a flow disregarding outcome of the conditional" in {
    val cpg = code("""
        |x = 12
        |def woo(x)
        |  return x == 10
        |end
        |
        |if !woo x
        |  puts x
        |else
        |  puts "No"
        |end
        |
        |""".stripMargin)

    val source = cpg.literal.code("12").l
    val sink   = cpg.call.name("puts").argument(1).l
    sink.reachableByFlows(source).size shouldBe 1
  }

}
