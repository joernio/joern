package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

class CfgTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  "CPG for code with simple structures" should {
    val cpg = code("""
        |package mypkg
        |
        |fun sink(p: Int): Int {
        |  return p + 1;
        |}
        |
        |class ClassFoo {
        |  fun foo(x: Int, y: Int): Int {
        |    if (y < 10) {
        |      return 1;
        |    }
        |    if (x < 10) {
        |      sink(x);
        |    }
        |    println("foo");
        |    return 0;
        |  }
        |}
    """.stripMargin)

    "should find that sink is control dependent on condition" in {
      val controllers = cpg.call("sink").controlledBy.isCall.toSet
      controllers.map(_.code) should contain("y < 10")
      controllers.map(_.code) should contain("x < 10")
    }

    "should find that first if controls `sink`" in {
      cpg.controlStructure.condition.code("y < 10").controls.isCall.name("sink").l.size shouldBe 1
    }

    "should find sink(x) does not dominate anything" in {
      cpg.call("sink").dominates.l.size shouldBe 0
    }

    "should find sink(x) is dominated by `x<10` and `y < 10`" in {
      cpg.call("sink").dominatedBy.isCall.code.toSet shouldBe Set("x < 10", "y < 10")
    }

    "should find that println post dominates correct nodes" in {
      cpg.call("println").postDominates.size shouldBe 6
    }

    "should find that method does not post dominate anything" in {
      cpg.method("foo").postDominates.l.size shouldBe 0
    }
  }
}
