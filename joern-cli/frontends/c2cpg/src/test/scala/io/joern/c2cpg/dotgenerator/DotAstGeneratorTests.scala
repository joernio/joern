package io.joern.c2cpg.dotgenerator

import io.joern.c2cpg.testfixtures.CCodeToCpgSuite
import io.shiftleft.semanticcpg.language._

class DotAstGeneratorTests extends CCodeToCpgSuite {

  override val code: String =
    """| // A comment
       |int my_func(int x)
       |{
       |  int y = x * 2;
       |  if (y > 42) {
       |    return y;
       |  } else {
       |    return sqrt(y);
       |  }
       |}
       |
       |void boop() {
       |  printf("Boop!");
       |  return;
       |}
       |""".stripMargin

  "An AstDotGenerator" should {

    "generate dot graph" in {
      cpg.method.name("my_func").dotAst.l match {
        case x :: _ =>
          x.startsWith("digraph \"my_func\"") shouldBe true
          x.contains("""[label = "(CONTROL_STRUCTURE,if (y > 42),if (y > 42))" ]""") shouldBe true
          x.endsWith("}\n") shouldBe true
        case _ => fail()
      }
    }

    "allow selection method" in {
      cpg.method.name("boop").dotAst.l match {
        case x :: _ => x.startsWith("digraph \"boop\"") shouldBe true
        case _      => fail()
      }
    }

    "not include MethodParameterOut nodes" in {
      cpg.method.name("my_func").dotAst.l match {
        case x :: _ => x.contains("PARAM_OUT") shouldBe false
        case _      => fail()
      }
    }

    "allow plotting sub trees of methods" in {
      cpg.method.ast.isControlStructure.code(".*y > 42.*").dotAst.l match {
        case x :: _ =>
          x.contains("y > 42") shouldBe true
          x.contains("IDENTIFIER,y") shouldBe true
          x.contains("x * 2") shouldBe false
        case _ => fail()
      }
    }

  }
}
