package io.joern.c2cpg.io.dotgenerator

import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.shiftleft.semanticcpg.language.*

class DotCdgGeneratorTests extends DataFlowCodeToCpgSuite {

  "DotCdgGeneratorTest1" should {
    // Test for https://github.com/joernio/joern/issues/1274
    val cpg = code("""
      |int foo(int x) {
      |  if(x > 8)
      |    int z = a(x);
      |}
      |""".stripMargin)

    "A CdgDotGenerator" should {
      "create correct dot graph for if-then without explicit block statement and a declaration" in {
        inside(cpg.method.name("foo").dotCdg.l) { case List(x) =>
          x should (
            startWith("digraph \"foo\"") and
              include("""[label = <&lt;operator&gt;.greaterThan, 3<BR/>x &gt; 8> ]""") and
              include("""[label = <&lt;operator&gt;.assignment, 4<BR/>z = a(x)> ]""") and
              include("""[label = <a, 4<BR/>a(x)> ]""") and
              endWith("}\n")
          )
          val lines = x.split("\n")
          lines.count(x => x.contains("->")) shouldBe 2
        }
      }
    }

  }

  "DotCdgGeneratorTest2" should {
    // Test for https://github.com/joernio/joern/issues/1274
    val cpg = code("""
      |int foo(int x) {
      |  if(x > 8)
      |    z = a(x);
      |}
      |""".stripMargin)

    "A CdgDotGenerator" should {
      "create correct dot graph for if-then without explicit block statement and an assignment" in {
        inside(cpg.method.name("foo").dotCdg.l) { case List(x) =>
          x should (
            startWith("digraph \"foo\"") and
              include("""[label = <&lt;operator&gt;.greaterThan, 3<BR/>x &gt; 8> ]""") and
              include("""[label = <&lt;operator&gt;.assignment, 4<BR/>z = a(x)> ]""") and
              include("""[label = <a, 4<BR/>a(x)> ]""") and
              endWith("}\n")
          )
          val lines = x.split("\n")
          lines.count(x => x.contains("->")) shouldBe 2
        }
      }
    }

  }

}
