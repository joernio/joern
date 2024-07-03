package io.joern.ghidra2cpg.querying.x86

import io.joern.ghidra2cpg.fixtures.GhidraBinToCpgSuite
import io.shiftleft.semanticcpg.language.*

class ParameterNodeTests extends GhidraBinToCpgSuite {

  override def beforeAll(): Unit = {
    super.beforeAll()
    buildCpgForBin("linux/x86/64/x86_64.bin")
  }

  "should contain at least one nodes with all mandatory fields set" in {
    cpg.method.name("printf").parameter.order(1).name.l match {
      case List(x, y) =>
        (x, y) shouldBe ("__format", "__format")
      case _ => fail()
    }
  }
}
