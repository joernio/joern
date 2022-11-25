package io.joern.c2cpg.io.dotgenerator

import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.joern.dataflowengineoss.dotgenerator.DdgGenerator
import io.joern.dataflowengineoss.language._
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Call, CfgNode, Method}
import io.shiftleft.semanticcpg.language._

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
        lines.count(x => x.contains("->")) shouldBe 31
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

  def dumpDDg(method: Method): String = {
    def printNode(n: AstNode): String = s"${n.label}:[${n.code}]@${n.lineNumber.getOrElse(-1)}"

    method.ast.collectAll[CfgNode].map { n => (n, n.ddgIn.l) }.collect { case (node, ddgIn) if ddgIn.nonEmpty => printNode(node) + " <- " + ddgIn.map(printNode).mkString(", ") }.mkString("\n")
  }

  "Compute dependencies according to Kuck et al 81 (Dependence Graphs and Compiler Optimizations)" should {
    //cf https://dl.acm.org/doi/pdf/10.1145/567532.567555
    // Note that this test is descriptive, not normative: It only documents current behavior and makes no judgement whether
    // output, or anti, or even input dependencies should be included in the DDG.
    "include flow-dependencies" in {
      val cpg = code(
        """
          |int fun(int* p)
          |{
          | int tmp = *p;
          | *p = tmp + 1;
          | return 0;
          |}
          |""".stripMargin)
      dumpDDg(cpg.method.name("fun").head) shouldBe
        """CALL:[tmp = *p]@4 <- IDENTIFIER:[tmp]@4
          |IDENTIFIER:[tmp]@4 <- CALL:[*p]@4
          |IDENTIFIER:[p]@4 <- METHOD_PARAMETER_IN:[int* p]@2
          |CALL:[*p = tmp + 1]@5 <- CALL:[*p]@5, IDENTIFIER:[tmp]@4
          |CALL:[*p]@5 <- CALL:[tmp + 1]@5
          |IDENTIFIER:[p]@5 <- METHOD_PARAMETER_IN:[int* p]@2
          |CALL:[tmp + 1]@5 <- IDENTIFIER:[tmp]@4
          |IDENTIFIER:[tmp]@5 <- IDENTIFIER:[tmp]@4
          |RETURN:[return 0;]@6 <- LITERAL:[0]@6
          |METHOD_RETURN:[int]@2 <- RETURN:[return 0;]@6, METHOD_PARAMETER_IN:[int* p]@2, CALL:[*p]@5, IDENTIFIER:[tmp]@5, CALL:[tmp + 1]@5
          |METHOD_PARAMETER_OUT:[int* p]@2 <- METHOD_PARAMETER_IN:[int* p]@2, CALL:[*p]@5""".stripMargin
      //we see that there is a ddgIn edge to the store (line 5) via identifier tmp from the load (line 4)
    }

    "Not include output-dependencies" in {
      val cpg = code(
        """
          |int fun(int* p)
          |{
          | *p = 0;
          | *p = 1;
          | return 0;
          |}
          |""".stripMargin)
      dumpDDg(cpg.method.name("fun").head) shouldBe
      """CALL:[*p = 0]@4 <- CALL:[*p]@4
        |CALL:[*p]@4 <- LITERAL:[0]@4
        |IDENTIFIER:[p]@4 <- METHOD_PARAMETER_IN:[int* p]@2
        |CALL:[*p = 1]@5 <- CALL:[*p]@5
        |CALL:[*p]@5 <- LITERAL:[1]@5
        |IDENTIFIER:[p]@5 <- METHOD_PARAMETER_IN:[int* p]@2, CALL:[*p]@4
        |RETURN:[return 0;]@6 <- LITERAL:[0]@6
        |METHOD_RETURN:[int]@2 <- RETURN:[return 0;]@6, METHOD_PARAMETER_IN:[int* p]@2, CALL:[*p]@5
        |METHOD_PARAMETER_OUT:[int* p]@2 <- METHOD_PARAMETER_IN:[int* p]@2, CALL:[*p]@5""".stripMargin
      //we see that there is no ddgIn edge to the second store (line 5) from the first (line 4)
    }

    "Not include anti-dependencies" in {
      val cpg = code(
        """
          |int fun(int* p)
          |{
          | int tmp = *p;
          | *p = 1;
          | return tmp;
          |}
          |""".stripMargin)
      dumpDDg(cpg.method.name("fun").head) shouldBe
        """CALL:[tmp = *p]@4 <- IDENTIFIER:[tmp]@4
          |IDENTIFIER:[tmp]@4 <- CALL:[*p]@4
          |IDENTIFIER:[p]@4 <- METHOD_PARAMETER_IN:[int* p]@2
          |CALL:[*p = 1]@5 <- CALL:[*p]@5
          |CALL:[*p]@5 <- LITERAL:[1]@5
          |IDENTIFIER:[p]@5 <- METHOD_PARAMETER_IN:[int* p]@2
          |RETURN:[return tmp;]@6 <- IDENTIFIER:[tmp]@6, IDENTIFIER:[tmp]@4
          |METHOD_RETURN:[int]@2 <- RETURN:[return tmp;]@6, METHOD_PARAMETER_IN:[int* p]@2, IDENTIFIER:[tmp]@4, CALL:[*p]@5
          |METHOD_PARAMETER_OUT:[int* p]@2 <- METHOD_PARAMETER_IN:[int* p]@2, CALL:[*p]@5""".stripMargin
      //we see that there is no ddgIn path to the store (line 5) from the load (line 4)
    }
    "Not include input dependencies" in {
      val cpg = code(
        """
          |int fun(volatile int* p)
          |{
          | int tmp = *p;
          | int tmp2 = *p;
          | return tmp + tmp2;
          |}
          |""".stripMargin)
      dumpDDg(cpg.method.name("fun").head) shouldBe
        """CALL:[tmp = *p]@4 <- IDENTIFIER:[tmp]@4
          |IDENTIFIER:[tmp]@4 <- CALL:[*p]@4
          |IDENTIFIER:[p]@4 <- METHOD_PARAMETER_IN:[volatile int* p]@2
          |CALL:[tmp2 = *p]@5 <- IDENTIFIER:[tmp2]@5
          |IDENTIFIER:[tmp2]@5 <- CALL:[*p]@5
          |IDENTIFIER:[p]@5 <- METHOD_PARAMETER_IN:[volatile int* p]@2
          |RETURN:[return tmp + tmp2;]@6 <- CALL:[tmp + tmp2]@6
          |CALL:[tmp + tmp2]@6 <- IDENTIFIER:[tmp]@4, IDENTIFIER:[tmp2]@5
          |IDENTIFIER:[tmp]@6 <- IDENTIFIER:[tmp]@4
          |IDENTIFIER:[tmp2]@6 <- IDENTIFIER:[tmp2]@5
          |METHOD_RETURN:[int]@2 <- RETURN:[return tmp + tmp2;]@6, METHOD_PARAMETER_IN:[volatile int* p]@2, CALL:[*p]@5, IDENTIFIER:[tmp]@6, IDENTIFIER:[tmp2]@6, CALL:[tmp + tmp2]@6
          |METHOD_PARAMETER_OUT:[volatile int* p]@2 <- METHOD_PARAMETER_IN:[volatile int* p]@2, CALL:[*p]@5""".stripMargin
      //we see that there is no ddgIn path to the load (line 5) from the other load (line 4).
      /*FIXME
         Choose a more instructive example, volatile has too crazy semantics. But we want an example that also works if we
         use an optimizing compiler and then use llvm2cpg or ghidra2cpg. This is conceptual and not about the C frontend.
         Without volatile, an optimizing compiler would load from memory once and then re-use the register value.
         With volatile, the two loads are IO operations and are ordered (i.e. one could argue that they are flow-dependent
         via global state. Volatile reads can have side-effects! We are not modeling that in c2cpg, though).
       */
    }
  }

}
