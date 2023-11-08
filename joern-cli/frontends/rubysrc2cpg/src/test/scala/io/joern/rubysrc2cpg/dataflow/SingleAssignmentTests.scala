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
    val source = cpg.literal
    val sink   = cpg.method.name("puts").callIn.argument
    val flows  = sink.reachableByFlows(source)
    flows.map(flowToResultPairs).toSet shouldBe
      Set(
        List(("y = 1", 2), ("y", 2), ("y = 1", 2), ("x", 2), ("x = y = 1", 2), ("z", 2), ("puts z", 5)),
        List(("y = 1", 2), ("y", 2), ("y = 1", 2), ("x", 2), ("puts x", 4)),
        List(("y = 1", 2), ("y", 2), ("puts y", 3))
      )
  }
}
