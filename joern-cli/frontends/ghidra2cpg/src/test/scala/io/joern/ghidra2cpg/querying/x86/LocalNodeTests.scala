package io.joern.ghidra2cpg.querying.x86

import io.joern.ghidra2cpg.fixtures.GhidraBinToCpgSuite
import io.shiftleft.semanticcpg.language.*

class LocalNodeTests extends GhidraBinToCpgSuite {

  override def beforeAll(): Unit = {
    super.beforeAll()
    buildCpgForBin("linux/x86/64/x86_64.bin")
  }

  "should contain exactly one node with all mandatory fields set" in {
    val x = cpg.method.name("localNodeTests").local.l.head
    x.name shouldBe "local_c"
    x.code shouldBe "[undefined4 local_c@Stack[-0xc]:4]"
    x.typeFullName shouldBe "undefined4"
  }
}
