package io.joern.ghidra2cpg.querying.mips

import io.joern.ghidra2cpg.fixtures.GhidraBinToCpgSuite
import io.shiftleft.semanticcpg.language.{ICallResolver, _}

class CallNodeTests extends GhidraBinToCpgSuite {

  override def beforeAll(): Unit = {
    super.beforeAll()
    buildCpgForBin("linux/mips/backdoor.mips")
  }

  "CPG for binary of a simple program should not contain any CALL nodes with more than two arguments and the same ARGUMENT_INDEX value" in {
    cpg.call
      .filter { c =>
        c.argument.size > 1 && c.argument.argumentIndex.toSet.size == 1
      }
      .code
      .l shouldBe List()
  }
}
