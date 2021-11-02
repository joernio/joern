package io.shiftleft.c2cpg.querying

import io.shiftleft.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.shiftleft.dataflowengineoss.language._
import io.shiftleft.semanticcpg.language._

class C2CpgDataFlowTests1 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      | int main(int x) {
      |  return x;
      |}
        """.stripMargin

  "Test ParameterToReturn1" should {
    "have a flow from input parameter to return" in {
      val source = cpg.method.name("main").parameter.name("x")
      val sink = cpg.method.name("main").methodReturn
      val flows = sink.reachableByFlows(source).l

      flows.map(flow => flowToResultPairs(flow)).toSet shouldBe
        Set(List(("main(int x)", Some(2)), ("return x;", Some(3)), ("int", Some(2))))
    }
  }

}

class C2CpgDataFlowTests2 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      | int main(int x) {
      |  int k = x + 1;
      |  int y = k + 2;
      |  return y + 3;
      |}
          """.stripMargin

  "Test ParameterToReturn2" should {
    "have a flow from input parameter to return" in {
      val source = cpg.method.name("main").parameter.name("x")
      val sink = cpg.method.name("main").methodReturn
      val flows = sink.reachableByFlows(source).l

      flows.map(flow => flowToResultPairs(flow)).toSet shouldBe
        Set(
          List(
            ("main(int x)", Some(2)),
            ("x + 1", Some(3)),
            ("k = x + 1", Some(3)),
            ("k + 2", Some(4)),
            ("y = k + 2", Some(4)),
            ("y + 3", Some(5)),
            ("return y + 3;", Some(5)),
            ("int", Some(2))
          ))
    }
  }
}

class C2CpgDataFlowTests3 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      | struct Point {
      |   int x;
      |   int y;
      | };
      |
      | double source () {
      |   return 2.0;
      | }
      |
      | int sink(int x) {
      |   return 3;
      | }
      |
      | void main() {
      |   int k = source(2);
      |   struct Point point;
      |   point.x = k;
      |   point.y = 2;
      |   sink(point.x);
      | }
          """.stripMargin

  "Test StructDataFlow" should {
    "have a flow from input parameter to return" in {
      val source = cpg.method.name("source").methodReturn
      val sink = cpg.method.name("sink").parameter.name("x")
      val flows = sink.reachableByFlows(source).l

      flows.map(flow => flowToResultPairs(flow)).toSet shouldBe
        Set(
          List(("double", Some(7)),
               ("source(2)", Some(16)),
               ("k = source(2)", Some(16)),
               ("point.x = k", Some(18)),
               ("sink(point.x)", Some(20)),
               ("sink(int x)", Some(11))))
    }
  }

}

class C2CpgDataFlowTests4 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      | int source() {
      |   return 2;
      | }
      |
      | int sink(int x) {
      |   return 3;
      | }
      |
      | void main() {
      |   int k = source();
      |   foo(k);
      | }
      |
      | void foo(int par) {
      |   sink(par);
      | }""".stripMargin

  "Test Interprocedural" should {
    "have a flow from input parameter to return" in {
      val source = cpg.method.name("source").methodReturn
      val sink = cpg.method.name("sink").parameter.name("x")
      val flows = sink.reachableByFlows(source).l

      pendingUntilFixed( // for whatever reason tracking to and from method foo fails.
        flows.map(flow => flowToResultPairs(flow)).toSet shouldBe
          Set(
            List(
              ("$ret", Some(2)),
              ("source(2)", Some(11)),
              ("p2", None),
              ("p1", None),
              ("k", Some(11)),
              ("k", Some(12)),
              ("par", Some(15)),
              ("par", Some(16)),
              ("x", Some(6))
            )
          ))
    }
  }

}

class C2CpgDataFlowTests5 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      | struct Point {
      |   int x;
      |   int y;
      | };
      |
      | struct Point source () {
      |   struct Point point;
      |   return point;
      | }
      |
      | int sink(int x) {
      |   return 0;
      | }
      |
      | void main() {
      |   struct Point point = source(2);
      |   sink(point.x);
      | }
          """.stripMargin

  "Test TaintedStruct" should {
    "have a flow from input parameter to return" in {
      val source = cpg.method.name("source").methodReturn
      val sink = cpg.method.name("sink").parameter.name("x")
      val flows = sink.reachableByFlows(source).l

      flows.map(flow => flowToResultPairs(flow)).toSet shouldBe
        Set(
          List(("struct Point", Some(7)),
               ("source(2)", Some(17)),
               ("point = source(2)", Some(17)),
               ("sink(point.x)", Some(18)),
               ("sink(int x)", Some(12))))
    }
  }

}

class C2CpgDataFlowTests6 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      | typedef struct {
      |   int len;
      |   int* buf;
      | } container;
      |
      | int source();
      | void sink(container* cont);
      |
      | void foo(container* c, int idx) {
      |   c->buf[idx] = source();
      |   c->buf = 0;
      |   sink(c);
      | }
          """.stripMargin

  "Overtaint behind exclusion" should {
    "not find any flows" in {
      val source = cpg.method.name("source").methodReturn
      val sink = cpg.method.name("sink").parameter
      val flows = sink.reachableByFlows(source).l
      flows.size shouldBe 0
    }
  }

}

class C2CpgDataFlowTests7 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      | int source();
      | void sink(int* cont);
      |
      | void foo(int** c, int idx) {
      |   c[1][2] = source();
      |   c[idx][2] = 0;
      |   sink(c[1]);
      | }
          """.stripMargin

  "Exclusions behind overtaint" should {
    "not kill flows" in {
      val source = cpg.method.name("source").methodReturn
      val sink = cpg.method.name("sink").parameter
      val flows = sink.reachableByFlows(source).l

      flows.map(flow => flowToResultPairs(flow)).toSet shouldBe Set(
        List(("int", Some(2)),
             ("source()", Some(6)),
             ("c[1][2] = source()", Some(6)),
             ("sink(c[1])", Some(8)),
             ("sink(int* cont)", Some(3))))
    }
  }

}

class C2CpgDataFlowTests8 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      |typedef struct {int field;} S;
      | int source();
      | void sink(int i);
      |
      | void foo(S* arg) {
      |   arg->field = source();
      |   sink((*arg).field);
      | }
          """.stripMargin

  "Pointer-to-struct, arrow vs star-dot" should {
    "actually find flows" in {
      val source = cpg.method.name("source").methodReturn
      val sink = cpg.method.name("sink").parameter
      val flows = sink.reachableByFlows(source).l

      flows.map(flow => flowToResultPairs(flow)).toSet shouldBe Set(
        List(("int", Some(3)),
             ("source()", Some(7)),
             ("arg->field = source()", Some(7)),
             ("sink((*arg).field)", Some(8)),
             ("sink(int i)", Some(4))))
    }
  }

}

class C2CpgDataFlowTests9 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      | int source();
      | void sink(int i);
      |
      | void foo(int* arg) {
      |   arg[0] = source();
      |   sink(*arg);
      | }
          """.stripMargin

  "Pointer deref vs array access" should {
    "actually find flows" in {
      val source = cpg.method.name("source").methodReturn
      val sink = cpg.method.name("sink").parameter
      val flows = sink.reachableByFlows(source).l

      flows.map(flow => flowToResultPairs(flow)).toSet shouldBe Set(
        List(("int", Some(2)),
             ("source()", Some(6)),
             ("arg[0] = source()", Some(6)),
             ("sink(*arg)", Some(7)),
             ("sink(int i)", Some(3))))
    }
  }

}

class C2CpgDataFlowTests10 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      |#include <stdio.h>
      |#include <stdlib.h>
      |#include <sys/types.h>
      |#include <unistd.h>int
      |void main()
      |{
      |    int a = getpid();
      |    int b = 888;    if(a == 666)
      |    {
      |        a = a * 666;
      |        b = 999;
      |    }
      |    else
      |    {
      |        a = a * 777;
      |    }    return a;
      |}""".stripMargin

  "PathUnfolding with allFlows" should {
    //regression test for  https://github.com/ShiftLeftSecurity/product/issues/7017
    "work as expected" in {
      def source = cpg.call("getpid")
      def sink = cpg.ret
      val flows = sink.reachableByFlows(source).l

      flows.map(flow => flowToResultPairs(flow)).toSet shouldBe
        Set(
          List(("getpid()", Some(8)),
               ("a = getpid()", Some(8)),
               ("a == 666", Some(9)),
               ("a * 666", Some(11)),
               ("a = a * 666", Some(11)),
               ("return a;", Some(17))))
    }
  }

}

class C2CpgDataFlowTests11 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      | int source();
      | void sink(int arg);
      | void nop(int x) {return;};
      |
      |
      | void foo(int* c, int idx) {
      |   c[2] = source();
      |   nop(c[idx]);
      |   sink(c[1]);
      | }
          """.stripMargin

  "NOP on overtaint" should {
    "not widen the search" in {
      val source = cpg.method.name("source").methodReturn
      val sink = cpg.method.name("sink").parameter
      val flows = sink.reachableByFlows(source).l
      // Issue: at nop, we have overtaint. Hence we track both into and around the call.
      // The into-part returns unchanged, and is reconstructed with overtaint. Once we track c[?], we are lost.
      pendingUntilFixed(
        flows.map(flow => flowToResultPairs(flow)).toSet shouldBe Set(
          // bad flow. delete once fixed, here for documentation only
          List(("$ret", Some(2)),
               ("source()", Some(8)),
               ("p2", None),
               ("p1", None),
               ("c[2]", Some(8)),
               ("c[1]", Some(10)),
               ("arg", Some(3)))))

      pendingUntilFixed(
        { flows.size shouldBe 0 }
      )

    }
  }

}
