package io.joern.rubysrc2cpg.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class SingleAssignmentTests extends RubyCode2CpgFixture(withPostProcessing = true, withDataFlow = true) {

  "flow through two inline assignments `z = x = y = 1`" in {
    val cpg = code("""
        |z = x = y = 1
        |puts y
        |puts x
        |puts z
        |""".stripMargin)
    val source = cpg.literal.l
    val sink   = cpg.method.name("puts").callIn.argument.l
    val flows  = sink.reachableByFlows(source).l
    flows.size shouldBe 6
    val List(flow1, flow2, flow3, flow4, flow5, flow6) = flows.map(flowToResultPairs).distinct.sortBy(_.length).toList
    flow1 shouldBe List(("y = 1", 2), ("y", 2), ("puts y", 3))
    flow2 shouldBe List(("y = 1", 2), ("x", 2), ("puts x", 4))
    flow3 shouldBe List(("y = 1", 2), ("z", 2), ("puts z", 5))
    flow4 shouldBe List(("y = 1", 2), ("y", 2), ("y = 1", 2), ("x", 2), ("puts x", 4))
    flow5 shouldBe List(("y = 1", 2), ("x", 2), ("x = y = 1", 2), ("z", 2), ("puts z", 5))
    flow6 shouldBe List(("y = 1", 2), ("y", 2), ("y = 1", 2), ("x", 2), ("x = y = 1", 2), ("z", 2), ("puts z", 5))
  }
}
