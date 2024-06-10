package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.JimpleCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._

class CfgTests extends JimpleCode2CpgFixture {

  lazy val cpg: Cpg = code("""
      |class Foo {
      | int foo(int x, int y) {
      |  if (y < 10)
      |    return -1;
      |  if (x < 5) {
      |   sink(x);
      |  }
      |  System.out.println("foo");
      |  return 0;
      | }
      |
      | void sink(int x) {
      |   System.out.println(x);
      |   return;
      | }
      |}
    """.stripMargin).cpg

  "should find that sink is control dependent on condition" in {
    val controllers = cpg.call("sink").controlledBy.isCall.toSetMutable
    controllers.map(_.code) should contain("y >= 10")
    controllers.map(_.code) should contain("x >= 5")
  }

  // No control structure node on flat ast
  //  "should find that first if controls `sink`" in {
  //    cpg.controlStructure.condition.code("y < 10").controls.isCall.name("sink").l.size shouldBe 1
  //  }

  //  "should find sink(x) does not dominate anything" in {
  //    cpg.call("sink").dominates.l.size shouldBe 0
  //  }

  "should find sink(x) is dominated by `x < 5` and `y < 10`" in {
    cpg.call("sink").dominatedBy.isCall.code.toSetMutable shouldBe Set("x >= 5", "y >= 10")
  }

  //  "should find that println post dominates correct nodes" in {
  //    cpg.call("println").postDominates.size shouldBe 6
  //  }

  //  "should find that method does not post dominate anything" in {
  //    cpg.method("foo").postDominates.l.size shouldBe 0
  //  }

}
