package io.joern.fuzzyc2cpg.querying

import io.joern.fuzzyc2cpg.testfixtures.FuzzyCCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language._

class CfgTests extends FuzzyCCodeToCpgSuite {

  override val code =
    """
      | int foo(int y, int y) {
      |  if (y < 10)
      |    goto end;
      |  if (x < 10) {
      |    sink(x);
      |  }
      |  end:
      |  printf("foo");
      | }
    """.stripMargin

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

  "should find that printf post dominates all" in {
    cpg.call("printf").postDominates.size shouldBe 12
  }

  "should find that method does not post dominate anything" in {
    cpg.method("foo").postDominates.l.size shouldBe 0
  }

  "should allow CFG successors to be filtered in if they pass a given node" in {
    val printf = cpg.method.call.name("printf").isCall
    val lt     = cpg.method.call.name(Operators.lessThan).isCall
    val sink   = cpg.method.call.name("sink").isCall
    // LTs always pass before printf
    lt.passes(printf).code.toSet shouldBe Set("y < 10", "x < 10")
    // printf does not pass before the LTs
    printf.passes(lt).code.toSet shouldBe Set()
    // "Foo" is after the call to "sink"
    sink.passes(cpg.literal("foo")).code.toSet shouldBe Set()
  }

  "should allow CFG successors to be filtered out if they pass a given node" in {
    val printf = cpg.method.call.name("printf").isCall
    val lt     = cpg.method.call.name(Operators.lessThan).isCall
    val sink   = cpg.method.call.name("sink").isCall
    // printf not before LTs
    lt.passesNot(printf).code.toSet shouldBe Set()
    // printf will always pass both of the LTs
    printf.passesNot(lt).code.toSet shouldBe Set()
    // "Foo" is after the call to "sink"
    sink.passesNot(cpg.literal("foo")).code.toSet shouldBe Set("sink(x)")
  }

}
