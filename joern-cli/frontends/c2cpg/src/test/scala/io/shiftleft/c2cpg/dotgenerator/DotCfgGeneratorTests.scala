package io.shiftleft.c2cpg.dotgenerator

import io.shiftleft.c2cpg.testfixtures.CCodeToCpgSuite
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
      cpg.method.name("main").dotCfg.l match {
        case x :: _ =>
          x.startsWith("digraph \"main\" {") shouldBe true
          x.contains("(<operator>.assignment,i = 0)") shouldBe true
          x.endsWith("}\n") shouldBe true
        case _ => fail()
      }
    }

    "not contain IDENTIFIER nodes" in {
      cpg.method.name("main").dotCfg.l match {
        case x :: _ =>
          x.contains("IDENTIFIER") shouldBe false
        case _ => fail()
      }
    }

    "contain seven nodes" in {
      val dotStr = cpg.method.name("main").dotCfg.head
      dotStr.split("\n").count(x => x.contains("label")) shouldBe 7
    }

    "contain seven edges" in {
      val dotStr = cpg.method.name("main").dotCfg.head
      dotStr.split("\n").count(x => x.contains("->")) shouldBe 7
    }

  }

}
