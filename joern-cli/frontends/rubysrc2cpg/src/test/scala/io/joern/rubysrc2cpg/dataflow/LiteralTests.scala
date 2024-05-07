package io.joern.rubysrc2cpg.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class LiteralTests extends RubyCode2CpgFixture(withPostProcessing = true, withDataFlow = true) {

  "Data flow through string interpolation" in {
    val cpg = code("""
                     |def foo(x)
                     |  str = "The source is #{x}"
                     |  puts str
                     |end
                     |""".stripMargin)

    val source = cpg.method("foo").parameter.index(1).l
    val sink   = cpg.call.name("puts").argument(1).l
    sink.reachableByFlows(source).size shouldBe 1
  }

  "flow through interpolated double-quoted symbol literal" in {
    val cpg = code("""def foo(x)
                     |  y = :"bar #{x}"
                     |  puts y
                     |end
                     |""".stripMargin)

    val source = cpg.method("foo").parameter.index(1).l
    val sink   = cpg.call.name("puts").argument(1).l
    sink.reachableByFlows(source).size shouldBe 1
  }

  "flow through symbol literal defined using \\:" in {
    val cpg = code("""
                     |def foo(arg)
                     |  hash = {:y => arg}
                     |  puts hash
                     |end
                     |""".stripMargin)

    val source = cpg.method("foo").parameter.index(1).l
    val sink   = cpg.call.name("puts").argument(1).l
    sink.reachableByFlows(source).size shouldBe 1
  }
}
