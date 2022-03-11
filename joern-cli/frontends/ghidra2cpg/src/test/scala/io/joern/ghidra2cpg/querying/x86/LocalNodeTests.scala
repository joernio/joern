package io.joern.ghidra2cpg.querying.x86

import io.joern.ghidra2cpg.fixtures.GhidraBinToCpgSuite
import io.shiftleft.semanticcpg.language._

class LocalNodeTests extends GhidraBinToCpgSuite {

  override def beforeAll(): Unit = {
    super.beforeAll()
    buildCpgForBin("linux/x86/64/x86_64.bin")
  }

  "should contain exactly one node with all mandatory fields set" in {
    val local = cpg.method.name("localNodeTests").local.l.head
    local.name shouldBe "local_10"
    local.code shouldBe "[undefined8 local_10@Stack[-0x10]:8]"
    local.typeFullName shouldBe "undefined8"
  }
}
