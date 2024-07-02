package io.joern.ghidra2cpg.querying.x86

import io.joern.ghidra2cpg.fixtures.GhidraBinToCpgSuite
import io.shiftleft.semanticcpg.language.*

class RefNodeTests extends GhidraBinToCpgSuite {

  override def beforeAll(): Unit = {
    super.beforeAll()
    buildCpgForBin("linux/x86/64/x86_64.bin")
  }

  "should contain exactly one local with one referencing identifier " in {
    cpg.method.name("refNodeTests").local.referencingIdentifiers.l match {
      case List(x, y) =>
        x.code shouldBe "local_c"
        y.code shouldBe "local_10"
      case _ => fail()
    }
  }
}
