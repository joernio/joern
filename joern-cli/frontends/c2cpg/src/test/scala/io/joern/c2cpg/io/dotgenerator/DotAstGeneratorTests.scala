package io.joern.c2cpg.io.dotgenerator

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.semanticcpg.language.*

class DotAstGeneratorTests extends C2CpgSuite {

  private val cpg = code("""| // A comment
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
       |
       |void lemon(){
       |  goog("\"yes\"");
       |}
       |""".stripMargin)

  "An AstDotGenerator" should {

    "generate dot graph" in {
      inside(cpg.method.name("my_func").dotAst.l) { case List(x) =>
        x should (
          startWith("digraph \"my_func\"") and
            include("""[label = <(CONTROL_STRUCTURE,IF,if (y &gt; 42))<SUB>5</SUB>> ]""") and
            endWith("}\n")
        )
      }
    }

    "allow selection method" in {
      inside(cpg.method.name("boop").dotAst.l) { case List(x) =>
        x should startWith("digraph \"boop\"")
      }
    }

    "not include MethodParameterOut nodes" in {
      inside(cpg.method.name("my_func").dotAst.l) { case List(x) =>
        x should not include "PARAM_OUT"
      }
    }

    "allow plotting sub trees of methods" in {
      inside(cpg.method.ast.isControlStructure.code(".*y > 42.*").dotAst.l) { case List(x, _) =>
        x should (include("y &gt; 42") and include("IDENTIFIER,y") and not include "x * 2")
      }
    }

    "allow plotting sub trees of methods correctly escaped" in {
      inside(cpg.method.name("lemon").dotAst.l) { case List(x) =>
        x should (
          startWith("digraph \"lemon\"") and
            include("""[label = <(goog,goog(&quot;\&quot;yes\&quot;&quot;))<SUB>18</SUB>> ]""") and
            include(
              """[label = <(LITERAL,&quot;\&quot;yes\&quot;&quot;,goog(&quot;\&quot;yes\&quot;&quot;))<SUB>18</SUB>> ]"""
            ) and
            endWith("}\n")
        )
      }
    }

  }
}
