package io.joern.ghidra2cpg.querying.x86

import io.joern.ghidra2cpg.fixtures.GhidraBinToCpgSuite
import io.shiftleft.semanticcpg.language._

class ParameterNodeTests extends GhidraBinToCpgSuite {

  override def beforeAll(): Unit = {
    super.beforeAll()
    buildCpgForBin("x86_64.bin")
  }

  "should contain atLeast one nodes with all mandatory fields set" in {
    cpg.method.name("printf").parameter.order(1).name.l match {
      case List(x, y, z) =>
        (x, y, z) shouldBe ("__format", "__format", "p1")
      case _ => fail()
    }
  }
}
