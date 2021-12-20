package io.joern.c2cpg.dotgenerator

import io.joern.c2cpg.testfixtures.CCodeToCpgSuite
import io.shiftleft.semanticcpg.language._

class DotCfgGeneratorTests extends CCodeToCpgSuite {

  override val code: String =
    """
      |int main(int argc, char **argv) {
      |   int i = 0;
      |   while(i < 10) {
      |     printf("Hello World");
      |     i++;
      |   }
      |   return 0;
      |}
      |""".stripMargin

  "A CfgDotGenerator" should {

    "create a dot graph" in {
      inside(cpg.method.name("main").dotCfg.l) { case List(x) =>
        x should (
          startWith("digraph \"main\" {") and
            include("(<operator>.assignment,i = 0)") and
            endWith("}\n")
        )
      }
    }

    "not contain IDENTIFIER nodes" in {
      inside(cpg.method.name("main").dotCfg.l) { case List(x) =>
        x should not include "IDENTIFIER"
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

}
