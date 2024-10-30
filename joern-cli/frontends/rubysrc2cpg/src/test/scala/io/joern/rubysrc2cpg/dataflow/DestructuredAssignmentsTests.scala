package io.joern.rubysrc2cpg.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class DestructuredAssignmentsTests extends RubyCode2CpgFixture(withPostProcessing = true, withDataFlow = true) {
  "Data flow through packing left hand side through the first identifier" in {
    val cpg = code("""
                     |x = 1
                     |p = 2
                     |*y = x,p
                     |puts y
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  "Data flow through packing left hand side through beyond the first identifier" in {
    val cpg = code("""
                     |x = 1
                     |y = 2
                     |z = 3
                     |*a = z,y,x
                     |puts a
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  "Data flow through packing left hand side with unequal RHS" in {
    val cpg = code("""
                     |x = 1
                     |y = 2
                     |z = 3
                     |p,*a = z,y,x
                     |puts a
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  "Data flow through single LHS and multiple RHS" in {
    val cpg = code("""
                     |x = 1
                     |y = 2
                     |z = 3
                     |a = z,y,x
                     |puts a
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }
}
