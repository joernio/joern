package io.joern.rubysrc2cpg.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class HashTests extends RubyCode2CpgFixture(withPostProcessing = true, withDataFlow = true) {
  // Works in deprecated
  "Data flow through hash constructor" ignore {
    val cpg = code("""
                     |def foo(arg)
                     |hash = {1 => arg, 2 => arg}
                     |puts hash
                     |end
                     |
                     |x = 3
                     |foo(x)
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).l.size shouldBe 2
  }

  // Works in deprecated - syntax error on new frontend
  "flow through hash containing splatting literal" ignore {
    val cpg = code("""
                     |x={:y=>1}
                     |z = {
                     |**x
                     |}
                     |puts z
                     |""".stripMargin)
    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }
}
