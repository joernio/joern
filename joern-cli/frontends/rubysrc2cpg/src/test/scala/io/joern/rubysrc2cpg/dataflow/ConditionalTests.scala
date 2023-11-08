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
    flows.map(flowToResultPairs).toSet shouldBe
      Set(
        List(("x = 1", 2), ("x", 2), ("foo ? x : y", 4), ("z", 4), ("puts z", 5)),
        List(("y = 2", 3), ("y", 3), ("foo ? x : y", 4), ("z", 4), ("puts z", 5))
      )
  }
}
