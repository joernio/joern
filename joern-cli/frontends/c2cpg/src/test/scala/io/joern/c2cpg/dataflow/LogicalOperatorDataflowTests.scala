package io.joern.c2cpg.dataflow

import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.joern.dataflowengineoss.language.*
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier}
import io.shiftleft.semanticcpg.language.*

class LogicalOperatorDataflowTests extends DataFlowCodeToCpgSuite {

  "operands of a logical AND in a condition" should {
    val cpg = code("""
        |void print(char *);
        |
        |int main() {
        |  int a = 1;
        |  int b = 2;
        |  if (a && b) {
        |    print("x");
        |  }
        |}""".stripMargin)

    "not depend on each other" in {
      val a = cpg.identifier("a").lineNumber(7).head
      val b = cpg.identifier("b").lineNumber(7).head

      a.ddgIn.collectAll[Identifier].map(id => (id.name, id.lineNumber.get)).l shouldBe List(("a", 5))
      b.ddgIn.collectAll[Identifier].map(id => (id.name, id.lineNumber.get)).l shouldBe List(("b", 6))
    }

    "still let both operands flow into the logical expression" in {
      val andCall = cpg.call.nameExact("<operator>.logicalAnd").head
      andCall.ddgIn.collectAll[Identifier].map(_.name).toSet shouldBe Set("a", "b")
    }
  }

  "operands of a logical OR in a condition" should {
    val cpg = code("""
        |void print(char *);
        |
        |int main() {
        |  int a = 1;
        |  int b = 2;
        |  if (a || b) {
        |    print("x");
        |  }
        |}""".stripMargin)

    "not depend on each other" in {
      val a = cpg.identifier("a").lineNumber(7).head
      val b = cpg.identifier("b").lineNumber(7).head

      a.ddgIn.collectAll[Identifier].map(id => (id.name, id.lineNumber.get)).l shouldBe List(("a", 5))
      b.ddgIn.collectAll[Identifier].map(id => (id.name, id.lineNumber.get)).l shouldBe List(("b", 6))
    }
  }

  "a value defined by a logical AND" should {
    val cpg = code("""
        |int main() {
        |  int a = 1;
        |  int b = 2;
        |  int c = a && 0;
        |  int d = 1 && b;
        |  return c + d;
        |}""".stripMargin)

    "still depend on the left operand" in {
      val flows = cpg.identifier("c").lineNumber(5).reachableByFlows(cpg.literal("1")).map(flowToResultPairs).l
      flows shouldBe List(List(("a = 1", 3), ("a && 0", 5), ("c = a && 0", 5)))
    }

    "still depend on the right operand" in {
      val flows = cpg.identifier("d").lineNumber(6).reachableByFlows(cpg.literal("2")).map(flowToResultPairs).l
      flows shouldBe List(List(("b = 2", 4), ("1 && b", 6), ("d = 1 && b", 6)))
    }
  }

  "a call as the right operand of a logical AND" should {
    val cpg = code("""
        |int check(int);
        |
        |int main() {
        |  int a = 1;
        |  int b = 2;
        |  int c = a && check(b);
        |  return c;
        |}""".stripMargin)

    "not make the left operand depend on the call" in {
      val a = cpg.identifier("a").lineNumber(7).head
      a.ddgIn.collectAll[Call].isEmpty shouldBe true
    }
  }
}
