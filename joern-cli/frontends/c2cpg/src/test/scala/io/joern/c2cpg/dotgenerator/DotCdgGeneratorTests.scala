package io.joern.c2cpg.dotgenerator

import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.shiftleft.semanticcpg.language._

class DotCdgGeneratorTest1 extends DataFlowCodeToCpgSuite {

  // Test for https://github.com/joernio/joern/issues/1274

  override val code: String =
    """
      |int foo(int x) {
      |  if(x > 8)
      |    int z = a(x);
      |}
      |""".stripMargin

  "A CdgDotGenerator" should {
    "create correct dot graph for if-then without explicit block statement and a declaration" in {
      inside(cpg.method.name("foo").dotCdg.l) { case List(x) =>
        x should (
          startWith("digraph \"foo\"") and
            include("""[label = "(<operator>.greaterThan,x > 8)" ]""") and
            include("""[label = "(<operator>.assignment,z = a(x))" ]""") and
            include("""[label = "(a,a(x))" ]""") and
            endWith("}\n")
        )
        val lines = x.split("\n")
        lines.count(x => x.contains("->")) shouldBe 2
      }
    }
  }

}

class DotCdgGeneratorTest2 extends DataFlowCodeToCpgSuite {

  // Test for https://github.com/joernio/joern/issues/1274

  override val code: String =
    """
      |int foo(int x) {
      |  if(x > 8)
      |    z = a(x);
      |}
      |""".stripMargin

  "A CdgDotGenerator" should {
    "create correct dot graph for if-then without explicit block statement and an assignment" in {
      inside(cpg.method.name("foo").dotCdg.l) { case List(x) =>
        x should (
          startWith("digraph \"foo\"") and
            include("""[label = "(<operator>.greaterThan,x > 8)" ]""") and
            include("""[label = "(<operator>.assignment,z = a(x))" ]""") and
            include("""[label = "(a,a(x))" ]""") and
            endWith("}\n")
        )
        val lines = x.split("\n")
        lines.count(x => x.contains("->")) shouldBe 2
      }
    }
  }

}
