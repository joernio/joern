package io.joern.rubysrc2cpg.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class RegexTests extends RubyCode2CpgFixture(withPostProcessing = true, withDataFlow = true) {
  // Works in deprecated - could not represent expression /x#{x}b/ in new frontend
  "Data flow through a regex interpolation" ignore {
    val cpg = code(s"""
                      |x="abc"
                      |y=/x#{x}b/
                      |puts y
                      |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  // Works in deprecated - could not represent expression /x#{x}b#{x+'z}... in new frontend
  "flow through a regex interpolation with multiple expressions" ignore {
    val cpg = code("""
                     |x="abc"
                     |y=/x#{x}b#{x+'z'}b{x+'y'+'z'}w/
                     |puts y
                     |""".stripMargin)
    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 3
  }

  "flow through statement when regular expression literal passed after `when`" in {
    val cpg = code("""
                     |x = 2
                     |a = 2
                     |
                     |case a
                     | when /^ch/
                     |   b = x
                     |   puts b
                     |end
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

}
