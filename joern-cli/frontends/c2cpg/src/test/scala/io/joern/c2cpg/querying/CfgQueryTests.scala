package io.joern.c2cpg.querying

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.semanticcpg.language.*

class CfgQueryTests extends C2CpgSuite {

  private val cpg = code("""
      | int foo(int x, int y) {
      |  if (y < 10)
      |    goto end;
      |  if (x < 10) {
      |    sink(x);
      |  }
      |  end:
      |  printf("foo");
      | }
    """.stripMargin)

  "should find that sink is control dependent on condition" in {
    val controllers = cpg.call("sink").controlledBy.isCall.toSetMutable
    controllers.map(_.code) should contain("y < 10")
    controllers.map(_.code) should contain("x < 10")
  }

  "should find that first if controls `sink`" in {
    cpg.controlStructure.condition.code("y < 10").controls.isCall.name("sink").l.size shouldBe 1
  }

  "should find sink(x) does not dominate anything" in {
    cpg.call("sink").dominates.l.size shouldBe 0
  }

  "should find sink(x) is dominated by `x < 10` and `y < 10`" in {
    cpg.call("sink").dominatedBy.isCall.code.toSetMutable shouldBe Set("x < 10", "y < 10")
  }

  "should find that printf post dominates all" in {
    cpg.call("printf").postDominates.size shouldBe 12
  }

  "should find that method does not post dominate anything" in {
    cpg.method("foo").postDominates.l.size shouldBe 0
  }
}
