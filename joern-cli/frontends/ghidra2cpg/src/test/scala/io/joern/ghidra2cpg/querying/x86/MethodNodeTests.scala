package io.joern.ghidra2cpg.querying.x86

import io.joern.ghidra2cpg.fixtures.GhidraBinToCpgSuite
import io.shiftleft.semanticcpg.language._

class MethodNodeTests extends GhidraBinToCpgSuite {

  override def beforeAll(): Unit = {
    super.beforeAll()
    buildCpgForBin("x86_64.bin")
  }

  "should contain exactly one node with all mandatory fields set" in {
    cpg.method.name("main").l match {
      case List(x) =>
        x.name shouldBe "main"
      case _ => fail()
    }
  }
}
