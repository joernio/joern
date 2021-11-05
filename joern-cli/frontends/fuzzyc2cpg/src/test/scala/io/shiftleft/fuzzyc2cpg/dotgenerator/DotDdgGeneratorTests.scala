package io.shiftleft.fuzzyc2cpg.dotgenerator

import io.shiftleft.dataflowengineoss.language._
import io.shiftleft.fuzzyc2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.shiftleft.semanticcpg.language._

class DotDdgGeneratorTests extends DataFlowCodeToCpgSuite {
  override val code =
    """
      |int foo(int param1, char *param2) {
      |   int i = 0;
      |   while(i < 10) {
      |     char *boo = moo("%d\n", i + bar(i));
      |     printf(boo);
      |     i++;
      |   }
      |   return 0;
      |}
      |""".stripMargin

  "A DdgDotGenerator" should {
    "create a dot graph with 31 edges" in {
      implicit val s = semantics
      val lines = cpg.method.name("foo").dotDdg.l.head.split("\n")
      lines.head.startsWith("digraph \"foo\"") shouldBe true
      lines.count(x => x.contains("->")) shouldBe 32
      lines.last.startsWith("}") shouldBe true
    }
  }
}

class DotDdgGeneratorTests2 extends DataFlowCodeToCpgSuite {
  override val code =
    """
      |int foo() {
      |int x = 42;
      |woo(x);
      |baz(x);
      |}
      |""".stripMargin

  "create correct dot graph" in {
    implicit val s = semantics
    val lines = cpg.method.name("foo").dotDdg.l.head.split("\n")
    lines.count(x => x.contains("->") && x.contains("\"x\"")) shouldBe 3
  }

}
