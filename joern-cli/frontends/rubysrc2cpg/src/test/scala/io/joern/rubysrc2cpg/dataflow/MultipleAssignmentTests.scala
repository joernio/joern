package io.joern.rubysrc2cpg.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class MultipleAssignmentTests extends RubyCode2CpgFixture(withPostProcessing = true, withDataFlow = true) {
  "flow through multiple assignments" in {
    val cpg = code("""
                     |x = 1
                     |y = 2
                     |c, d = x, y
                     |puts c
                     |""".stripMargin)

    val src  = cpg.identifier.name("x").l
    val sink = cpg.call.name("puts").l
    sink.reachableByFlows(src).l.size shouldBe 2
  }

  // Works in deprecated - fails to parse in new frontend
  "flow through multiple assignments with grouping" ignore {
    val cpg = code("""
                     |x = 1
                     |y = 2
                     |(c, d) = x, y
                     |puts c
                     |""".stripMargin)

    val src  = cpg.identifier.name("x").l
    val sink = cpg.call.name("puts").l
    sink.reachableByFlows(src).l.size shouldBe 2
  }

  // Works in deprecated - fails to parse in new frontend
  "Data flow through multiple assignments with grouping and method in RHS" ignore {
    val cpg = code("""
                     |def foo()
                     |x = 1
                     |return x
                     |end
                     |
                     |b = 2
                     |(c, d) = foo, b
                     |puts c
                     |
                     |""".stripMargin)

    val src  = cpg.identifier.name("x").l
    val sink = cpg.call.name("puts").l
    sink.reachableByFlows(src).l.size shouldBe 2
  }

  // Works in deprecated
  "Data flow through single LHS and splatting RHS" ignore {
    val cpg = code("""
                     |x=1
                     |y=*x
                     |puts y
                     |""".stripMargin)

    val src  = cpg.identifier.name("x").l
    val sink = cpg.call.name("puts").l
    sink.reachableByFlows(src).l.size shouldBe 2
  }

}
