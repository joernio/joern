package io.joern.c2cpg.dotgenerator

import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.semanticcpg.language._

class DotDdgGeneratorTest1 extends DataFlowCodeToCpgSuite {

  override val code: String =
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
    "create correct dot graph" in {
      implicit val s: Semantics = semantics
      inside(cpg.method.name("foo").dotDdg.l) { case List(elem) =>
        val lines = elem.split("\n")
        lines.head should startWith("digraph \"foo\"")
        lines.count(x => x.contains("->")) shouldBe 32
        lines.last should startWith("}")
      }
    }
  }
}

class DotDdgGeneratorTest2 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      |int foo() {
      |int x = 42;
      |woo(x);
      |baz(x);
      |}
      |""".stripMargin

  "A DdgDotGenerator" should {
    "create correct dot graph" in {
      implicit val s: Semantics = semantics
      inside(cpg.method.name("foo").dotDdg.l) { case List(elem) =>
        val lines = elem.split("\n")
        lines.count(x => x.contains("->") && x.contains("\"x\"")) shouldBe 3
      }
    }
  }

}
