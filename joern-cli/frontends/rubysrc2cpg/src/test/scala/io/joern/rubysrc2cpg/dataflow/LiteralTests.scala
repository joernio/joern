package io.joern.rubysrc2cpg.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class LiteralTests extends RubyCode2CpgFixture(withPostProcessing = true, withDataFlow = true) {
  "Data flow through string interpolation" in {
    val cpg = code("""
                     |x = 1
                     |str = "The source is #{x}"
                     |puts str
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).l.size shouldBe 2
  }

  // Works in deprecated - Could not represent expression x on new frontend
  "flow through interpolated double-quoted string literal " ignore {
    val cpg = code("""
                     |x = "foo"
                     |y = :"bar #{x}"
                     |puts y
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  // Works in deprecated
  "flow through symbol literal defined using \\:" ignore {
    val cpg = code("""
                     |def foo(arg)
                     |hash = {:y => arg}
                     |puts hash
                     |end
                     |
                     |x = 3
                     |foo(x)
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }
}
