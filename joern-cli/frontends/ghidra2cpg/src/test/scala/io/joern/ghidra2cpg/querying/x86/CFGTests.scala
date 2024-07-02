package io.joern.ghidra2cpg.querying.x86

import io.joern.ghidra2cpg.fixtures.GhidraBinToCpgSuite
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language.*

class CFGTests extends GhidraBinToCpgSuite {

  override def beforeAll(): Unit = {
    super.beforeAll()
    buildCpgForBin("linux/x86/64/cfg.bin")
  }

  "should have the cfgFirst node with the value set in" in {
    val cfgFirst = cpg.method.name("main").cfgFirst.l.head
    cfgFirst.code shouldBe "PUSH RBP"
    cfgFirst.order shouldBe 0
  }

  "should have correct CFG edge out of an unconditional jump" in {
    val jmp = cpg.method.name("main").call.code("JMP.*").l.head
    jmp.cfgNext.l match {
      case List(node: Call) =>
        node.code shouldBe "CMP RCX,0x0"

      case result =>
        fail(s"Expected single call node `CMP RCX, 0x0` but got $result")
    }
  }

  "should have correct CFG edges out of a conditional jump" in {
    val jmp = cpg.method.name("main").call.code("JLE.*").l.head
    jmp.cfgNext.l match {
      case List(nextIfSkipped: Call, nextIfTaken: Call) =>
        nextIfSkipped.code shouldBe "ADD RAX,0x2"
        nextIfTaken.code shouldBe "MOV RSP,RBP"
      case result =>
        fail(s"Expected call nodes `ADD RAX,0x2` and `MOV RSP,RBP` but got $result")
    }
  }
}
