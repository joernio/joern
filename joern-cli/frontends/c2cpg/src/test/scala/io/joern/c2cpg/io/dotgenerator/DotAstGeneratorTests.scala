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
            include(
              """[label = <CONTROL_STRUCTURE, 5<BR/>IF<BR/>if (y &gt; 42) { return y; } else { return sqrt(y); }> ]"""
            ) and
            include("""[label = <LITERAL, 5<BR/>42<BR/>y &gt; 42> ]""") and
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
        x should (include("y &gt; 42") and include("IDENTIFIER, 5<BR/>y") and not include "x * 2")
      }
    }

    "allow plotting sub trees of methods correctly escaped" in {
      inside(cpg.method.name("lemon").dotAst.l) { case List(x) =>
        x should (
          startWith("digraph \"lemon\"") and
            include("""[label = <goog, 18<BR/>goog(&quot;\&quot;yes\&quot;&quot;)> ]""") and
            include(
              """[label = <LITERAL, 18<BR/>&quot;\&quot;yes\&quot;&quot;<BR/>goog(&quot;\&quot;yes\&quot;&quot;)> ]"""
            ) and
            endWith("}\n")
        )
      }
    }

  }
}
