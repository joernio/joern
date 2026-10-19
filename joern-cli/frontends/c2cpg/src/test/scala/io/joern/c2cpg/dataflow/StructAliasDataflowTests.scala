package io.joern.c2cpg.dataflow

import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.joern.dataflowengineoss.language.*
import io.shiftleft.semanticcpg.language.*

class StructAliasDataflowTests extends DataFlowCodeToCpgSuite {

  "pointer field write read through the struct" should {
    val cpg = code("""
        |#include <stdio.h>
        |
        |struct numbers {
        |  int number_1;
        |  int number_2;
        |};
        |
        |int main(int argc, char **argv) {
        |  struct numbers nums = {};
        |  int one = 1;
        |  nums.number_1 = one;
        |  printf("%d", nums.number_1);
        |
        |  int two = 2;
        |  struct numbers *nums_p = &nums;
        |  nums_p->number_2 = two;
        |  printf("%d", nums.number_2);
        |  return 0;
        |}""".stripMargin)

    "flow from one through direct struct field access" in {
      val source = cpg.identifier.name("one")
      val sink   = cpg.call("printf").argument(2).where(_.code("nums.number_1"))
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSetMutable shouldBe Set(
        List(("nums.number_1 = one", 12), ("printf(\"%d\", nums.number_1)", 13)),
        List(("one = 1", 11), ("nums.number_1 = one", 12), ("printf(\"%d\", nums.number_1)", 13))
      )
    }

    "flow from two through pointer write and struct field read" in {
      val source = cpg.identifier.name("two")
      val sink   = cpg.call("printf").argument(2).where(_.code("nums.number_2"))
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSetMutable shouldBe Set(
        List(("nums_p->number_2 = two", 17), ("printf(\"%d\", nums.number_2)", 18)),
        List(("two = 2", 15), ("nums_p->number_2 = two", 17), ("printf(\"%d\", nums.number_2)", 18))
      )
    }
  }

  "struct write pointer field read" should {
    val cpg = code("""
        |#include <stdio.h>
        |
        |struct numbers {
        |  int number_1;
        |  int number_2;
        |};
        |
        |int main() {
        |  struct numbers nums = {};
        |  struct numbers *nums_p = &nums;
        |  int two = 2;
        |  nums.number_2 = two;
        |  printf("%d", nums_p->number_2);
        |  return 0;
        |}""".stripMargin)

    "flow from struct assignment to pointer field read" in {
      val source = cpg.identifier.name("two")
      val sink   = cpg.call("printf").argument(2).where(_.code("nums_p->number_2"))
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSetMutable shouldBe Set(
        List(("two = 2", 12), ("nums.number_2 = two", 13), ("printf(\"%d\", nums_p->number_2)", 14)),
        List(("nums.number_2 = two", 13), ("printf(\"%d\", nums_p->number_2)", 14))
      )
    }
  }

  "field sensitivity across pointer alias" should {
    val cpg = code("""
        |#include <stdio.h>
        |
        |struct numbers {
        |  int number_1;
        |  int number_2;
        |};
        |
        |int main() {
        |  struct numbers nums = {};
        |  struct numbers *nums_p = &nums;
        |  int two = 2;
        |  nums_p->number_2 = two;
        |  printf("%d", nums.number_1);
        |  return 0;
        |}""".stripMargin)

    "not flow into a different struct field" in {
      val source = cpg.identifier.name("two")
      val sink   = cpg.call("printf").argument(2).where(_.code("nums.number_1"))
      sink.reachableByFlows(source).size shouldBe 0
    }
  }

  "field sensitivity on simplified struct members" should {
    val cpg = code("""
        |struct Data { char *buf; int len; };
        |char *source();
        |void sink(char *str);
        |
        |int main() {
        |  struct Data d;
        |  d.buf = source();
        |  d.len = 10;
        |  sink(d.buf);
        |  return 0;
        |}""".stripMargin)

    "flow through buf only not len" in {
      val source = cpg.call("source")
      val sink   = cpg.call("sink").argument(1)
      sink.reachableByFlows(source).nonEmpty shouldBe true
    }

    "not flow from buf assignment into len" in {
      val source = cpg.call("source")
      val sink   = cpg.assignment.code(".*\\.len = .*").target
      sink.reachableByFlows(source).size shouldBe 0
    }
  }

  "reassignment drops stale pointer alias" should {
    val cpg = code("""
        |struct S1 { int f; };
        |struct S2 { int f; };
        |int source();
        |void sink(int i);
        |
        |void foo() {
        |  struct S1 s1;
        |  struct S2 s2;
        |  struct S1 *p;
        |  p = &s1;
        |  p = &s2;
        |  p->f = source();
        |  sink(s1.f);
        |}""".stripMargin)

    "not report flow after pointer reassignment" in {
      val source = cpg.call("source")
      val sink   = cpg.call("sink").argument(1)
      sink.reachableByFlows(source).size shouldBe 0
    }
  }

}
