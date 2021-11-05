package io.joern.ghidra2cpg.querying.x86

import io.joern.ghidra2cpg.fixtures.GhidraBinToCpgSuite
import io.shiftleft.semanticcpg.language._

class ReturnNodeTests extends GhidraBinToCpgSuite {

  override def beforeAll(): Unit = {
    super.beforeAll()
    buildCpgForBin("x86_64.bin")
  }

  "should contain exactly one node with all mandatory fields set" in {
    cpg.method.name("main").methodReturn.l match {
      case List(x) =>
        x.order shouldBe 1
      case _ => fail()
    }
  }
}
