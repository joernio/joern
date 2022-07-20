package io.joern.c2cpg.dotgenerator

import io.joern.c2cpg.testfixtures.CCodeToCpgSuite
import io.shiftleft.semanticcpg.language._

class DotCfgGeneratorTests extends CCodeToCpgSuite {

  "DotCfgGeneratorTest1" should {

    val cpg = code("""
      |int main(int argc, char **argv) {
      |   int i = 0;
      |   while(i < 10) {
      |     printf("Hello World");
      |     i++;
      |   }
      |   return 0;
      |}
      |""".stripMargin)

    "create a dot graph" in {
      inside(cpg.method.name("main").dotCfg.l) { case List(dotStr) =>
        dotStr should (
          startWith("digraph \"main\" {") and
            include("(&lt;operator&gt;.assignment,i = 0)") and
            endWith("}\n")
        )
      }
    }

    "not contain IDENTIFIER nodes" in {
      inside(cpg.method.name("main").dotCfg.l) { case List(dotStr) =>
        dotStr should not include "IDENTIFIER"
      }
    }

    "contain seven nodes" in {
      inside(cpg.method.name("main").dotCfg.l) { case List(dotStr) =>
        dotStr.split("\n").count(x => x.contains("label")) shouldBe 7
      }
    }

    "contain seven edges" in {
      inside(cpg.method.name("main").dotCfg.l) { case List(dotStr) =>
        dotStr.split("\n").count(x => x.contains("->")) shouldBe 7
      }
    }

  }

  "DotCfgGeneratorTest2" should {
    val cpg = code("""
      |bool a;
      |bool b;
      |
      |bool test() {
      |  return a ? a : b;
      |}""".stripMargin)

    "not contain the same edge more than once" in {
      inside(cpg.method.name("test").dotCfg.l) { case List(dotStr) =>
        val rawEdges   = dotStr.split("\n").filter(x => x.contains("->"))
        val dedupEdges = rawEdges.distinct
        rawEdges.length shouldBe dedupEdges.length
      }
    }

  }

}
