package io.joern.c2cpg.io.dotgenerator

import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.joern.dataflowengineoss.language.*
import io.shiftleft.semanticcpg.language.*

class DotDdgGeneratorTests extends DataFlowCodeToCpgSuite {

  "DotDdgGeneratorTest1" should {
    val cpg = code("""
      |int foo(int param1, char *param2) {
      |   int i = 0;
      |   while(i < 10) {
      |     char *boo = moo("%d\n", i + bar(i));
      |     printf(boo);
      |     i++;
      |   }
      |   return 0;
      |}
      |""".stripMargin)

    "create correct dot graph" in {
      inside(cpg.method.name("foo").dotDdg.l) { case List(elem) =>
        val lines = elem.split("\n")
        lines.head should startWith("digraph \"foo\"")
        lines.count(x => x.contains("->")) shouldBe 34
        lines.last should startWith("}")
      }
    }
  }

  "DotDdgGeneratorTest2" should {
    val cpg = code("""
      |int foo() {
      |int x = 42;
      |woo(x);
      |baz(x);
      |}
      |""".stripMargin)

    "A DdgDotGenerator" should {
      "create correct dot graph" in {
        inside(cpg.method.name("foo").dotDdg.l) { case List(elem) =>
          val lines = elem.split("\n")
          lines.count(x => x.contains("->") && x.contains("\"x\"")) shouldBe 3
        }
      }
    }
  }

}
