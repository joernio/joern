package io.joern.ghidra2cpg.querying.x86

import io.joern.ghidra2cpg.fixtures.GhidraBinToCpgSuite
import io.shiftleft.semanticcpg.language.*

class LiteralNodeTests extends GhidraBinToCpgSuite {

  override def beforeAll(): Unit = {
    super.beforeAll()
    buildCpgForBin("linux/x86/64/x86_64.bin")
  }

  "should contain exactly one literal node containing \"TEST\" with all mandatory fields set" in {
    cpg.literal.code("TEST").l match {
      case List(x) =>
        x.code shouldBe "TEST"
      case _ => fail()
    }
  }
  "should contain exactly one node with all mandatory fields set" in {
    // keep in mind: 0x64 = 100
    cpg.literal.code("64").l.nonEmpty
  }

  "should contain exactly one call with literal arguments" in {
    // keep in mind = 0x64
    val result = cpg.call.name("<operator>.assignment").argument.code("0x64").l
    result match {
      case List(x) =>
        x.code shouldBe "0x64"
      case _ => fail()
    }
  }
}
