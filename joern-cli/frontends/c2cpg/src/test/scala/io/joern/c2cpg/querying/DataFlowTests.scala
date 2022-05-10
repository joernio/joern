package io.joern.c2cpg.querying

import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.joern.dataflowengineoss.language._
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.Identifier
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.toNodeTraversal

class DataFlowTest1 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """| struct node {
       |  int value;
       |  struct node *next;
       | };
       |
       | void free_list(struct node *head) {
       |  struct node *q;
       |  for (struct node *p = head; p != NULL; p = q) {
       |    q = p->next;
       |    free(p);
       |  }
       | }
       | 
       | int flow(int p0) {
       |    int a = p0;
       |    int b=a;
       |    int c=0x31;
       |    int z = b + c;
       |    z++;
       |    int x = z;
       |    return x;
       | }""".stripMargin

  "should identify all calls to `free`" in {
    cpg.call.name("free").code.toSetMutable shouldBe Set("free(p)")
  }

  "should find flows to arguments of `free`" in {
    implicit val callResolver: NoResolve.type = NoResolve
    val source                                = cpg.identifier
    val sink                                  = cpg.method.name("free").parameter.argument
    sink.reachableByFlows(source).l.map(flowToResultPairs).distinct.size shouldBe 5
  }

  "should find flows to `free`" in {
    val source = cpg.identifier
    val sink   = cpg.call.name("free")
    sink.reachableByFlows(source).l.map(flowToResultPairs).distinct.size shouldBe 5
  }

  "should find flows from identifiers to return values of `flow`" in {
    val source = cpg.identifier
    val sink   = cpg.method.name("flow").methodReturn
    sink.reachableByFlows(source).l.map(flowToResultPairs).distinct.size shouldBe 9
  }

  "find flows from z to method returns of flow" in {
    val source = cpg.identifier.name("z")
    val sink   = cpg.method.name("flow").methodReturn
    sink.reachableByFlows(source).size shouldBe 3
  }
}

class DataFlowTest2 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      | int main(int x) {
      |  return x;
      |}
        """.stripMargin

  "Test ParameterToReturn1" should {
    "have a flow from input parameter to return" in {
      val source = cpg.method.name("main").parameter.name("x")
      val sink   = cpg.method.name("main").methodReturn
      val flows  = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSetMutable shouldBe
        Set(List(("main(int x)", Some(2)), ("return x;", Some(3)), ("int", Some(2))))
    }
  }

}

class DataFlowTest3 extends DataFlowCodeToCpgSuite {

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
      val sink   = cpg.method.name("main").methodReturn
      val flows  = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSetMutable shouldBe
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
          )
        )
    }
  }
}

class DataFlowTest4 extends DataFlowCodeToCpgSuite {

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
      val sink   = cpg.method.name("sink").parameter.name("x")
      val flows  = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSetMutable shouldBe
        Set(
          List(
            ("double", Some(7)),
            ("source(2)", Some(16)),
            ("k = source(2)", Some(16)),
            ("point.x = k", Some(18)),
            ("sink(point.x)", Some(20)),
            ("sink(int x)", Some(11))
          )
        )
    }
  }

}

class DataFlowTest5 extends DataFlowCodeToCpgSuite {

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
      val sink   = cpg.method.name("sink").parameter.name("x")
      val flows  = sink.reachableByFlows(source)

      pendingUntilFixed( // for whatever reason tracking to and from method foo fails.
        flows.map(flowToResultPairs).toSetMutable shouldBe
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
          )
      )
    }
  }

}

class DataFlowTest6 extends DataFlowCodeToCpgSuite {

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
      val sink   = cpg.method.name("sink").parameter.name("x")
      val flows  = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSetMutable shouldBe
        Set(
          List(
            ("struct Point", Some(7)),
            ("source(2)", Some(17)),
            ("point = source(2)", Some(17)),
            ("sink(point.x)", Some(18)),
            ("sink(int x)", Some(12))
          )
        )
    }
  }

}

class DataFlowTest7 extends DataFlowCodeToCpgSuite {

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
      val sink   = cpg.method.name("sink").parameter
      val flows  = sink.reachableByFlows(source)

      flows.size shouldBe 0
    }
  }

}

class DataFlowTest8 extends DataFlowCodeToCpgSuite {

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
      val sink   = cpg.method.name("sink").parameter
      val flows  = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSetMutable shouldBe Set(
        List(
          ("int", Some(2)),
          ("source()", Some(6)),
          ("c[1][2] = source()", Some(6)),
          ("sink(c[1])", Some(8)),
          ("sink(int* cont)", Some(3))
        )
      )
    }
  }

}

class DataFlowTest9 extends DataFlowCodeToCpgSuite {

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
      val sink   = cpg.method.name("sink").parameter
      val flows  = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSetMutable shouldBe Set(
        List(
          ("int", Some(3)),
          ("source()", Some(7)),
          ("arg->field = source()", Some(7)),
          ("sink((*arg).field)", Some(8)),
          ("sink(int i)", Some(4))
        )
      )
    }
  }

}

class DataFlowTest10 extends DataFlowCodeToCpgSuite {

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
      val sink   = cpg.method.name("sink").parameter
      val flows  = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSetMutable shouldBe Set(
        List(
          ("int", Some(2)),
          ("source()", Some(6)),
          ("arg[0] = source()", Some(6)),
          ("sink(*arg)", Some(7)),
          ("sink(int i)", Some(3))
        )
      )
    }
  }

}

class DataFlowTest11 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      |#include <stdio.h>
      |#include <stdlib.h>
      |#include <sys/types.h>
      |#include <unistd.h>
      |
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
    // regression test for  https://github.com/ShiftLeftSecurity/product/issues/7017
    "work as expected" in {
      def source = cpg.call("getpid")
      def sink   = cpg.ret
      val flows  = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSetMutable shouldBe
        Set(
          List(
            ("getpid()", Some(9)),
            ("a = getpid()", Some(9)),
            ("a == 666", Some(10)),
            ("a * 666", Some(12)),
            ("a = a * 666", Some(12)),
            ("return a;", Some(18))
          )
        )
    }
  }

}

class DataFlowTest12 extends DataFlowCodeToCpgSuite {

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
      val sink   = cpg.method.name("sink").parameter
      val flows  = sink.reachableByFlows(source)

      // Issue: at nop, we have overtaint. Hence we track both into and around the call.
      // The into-part returns unchanged, and is reconstructed with overtaint. Once we track c[?], we are lost.
      pendingUntilFixed(
        flows.map(flowToResultPairs).toSetMutable shouldBe Set(
          // bad flow. delete once fixed, here for documentation only
          List(
            ("$ret", Some(2)),
            ("source()", Some(8)),
            ("p2", None),
            ("p1", None),
            ("c[2]", Some(8)),
            ("c[1]", Some(10)),
            ("arg", Some(3))
          )
        )
      )

      pendingUntilFixed({ flows.size shouldBe 0 })

    }
  }

}

class DataFlowTest13 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      |
      | void flows1(FILE *fd, int mode) {
      |     char buff[40];
      |
      |     int sz = 0;
      |     if (mode == 1) sz = 20;
      |     if (mode == 2) sz = 200;
      |     if (mode == 3) sz = 41;
      |     if (mode == 5) sz = -5;
      |
      |     read(fd, buff, sz);
      | }
      """.stripMargin

  "flow from function call read to multiple versions of the same variable" in {
    def source = cpg.identifier.name("sz")
    def sink   = cpg.call.name("read")

    def flows = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(
        List[(String, Option[Integer])](("sz = 200", 8), ("read(fd, buff, sz)", 12)),
        List[(String, Option[Integer])](("sz = -5", 10), ("read(fd, buff, sz)", 12)),
        List[(String, Option[Integer])](("sz = 41", 9), ("read(fd, buff, sz)", 12)),
        List[(String, Option[Integer])](("sz = 0", 6), ("read(fd, buff, sz)", 12)),
        List[(String, Option[Integer])](("sz = 20", 7), ("read(fd, buff, sz)", 12)),
        List[(String, Option[Integer])](("read(fd, buff, sz)", 12))
      )

    // pretty printing for flows
    val flowsPretty = flows.p.mkString
    flowsPretty.should(include("sz = 20"))
    flowsPretty.should(include("read(fd, buff, sz)"))
    val tmpSourceFile = flows.head.elements.head.method.filename
    flowsPretty.should(include(tmpSourceFile))
  }
}

class DataFlowTest14 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      |struct node {
      | int value;
      | struct node *next;
      |};
      |void free_list(struct node *head) {
      | struct node *q;
      | for (struct node *p = head; p != NULL; p = q) {
      | q = p->next;
      | free(p);
      | }
      |}
      """.stripMargin

  "flow with pointers" in {
    implicit val callResolver: NoResolve.type = NoResolve

    val source = cpg.identifier
    val sink   = cpg.method.name("free").parameter.argument
    val flows  = sink.reachableByFlows(source).map(flowToResultPairs).l.distinct

    flows.size shouldBe 5

    flows.toSet shouldBe
      Set(
        List[(String, Option[Integer])](("*p = head", 8), ("p != NULL", 8), ("free(p)", 10)),
        List[(String, Option[Integer])](("q = p->next", 9), ("p = q", 8), ("p != NULL", 8), ("free(p)", 10)),
        List[(String, Option[Integer])](("p = q", 8), ("p != NULL", 8), ("free(p)", 10)),
        List[(String, Option[Integer])](("p != NULL", 8), ("free(p)", 10)),
        List[(String, Option[Integer])](("free(p)", 10))
      )
  }
}

class DataFlowTest15 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      | int method(int y){
      |  int a = 10;
      |  if (a < y){
      |    foo(a);
      |  }
      | }
      """.stripMargin

  "flow from function call argument" in {
    implicit val callResolver: NoResolve.type = NoResolve

    val source = cpg.identifier.name("a")
    val sink   = cpg.method.name("foo").parameter.argument
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(
        List[(String, Option[Integer])](("a = 10", 3), ("a < y", 4), ("foo(a)", 5)),
        List[(String, Option[Integer])](("a < y", 4), ("foo(a)", 5)),
        List[(String, Option[Integer])](("foo(a)", 5))
      )
  }
}

class DataFlowTest16 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      | void flow(void) {
      |   int a = 0x37;
      |   int b=a;
      |   int c=0x31;
      |   int z = b + c;
      |   z++;
      |   int* p = &z;
      |   int x = z;
      | }
      """.stripMargin

  "flow chains from x to a" in {
    val source = cpg.identifier.name("a")
    val sink   = cpg.identifier.name("x")
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(
        List[(String, Option[Integer])](
          ("a = 0x37", 3),
          ("b=a", 4),
          ("b + c", 6),
          ("z = b + c", 6),
          ("z++", 7),
          ("x = z", 9)
        ),
        List[(String, Option[Integer])](("b=a", 4), ("b + c", 6), ("z = b + c", 6), ("z++", 7), ("x = z", 9))
      )
  }
}

class DataFlowTest17 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      | int flow(int a){
      |   int z = a;
      |   int b = z;
      |
      |   return b;
      | }
      """.stripMargin

  "flow from method return to a" in {
    val source = cpg.identifier.name("a")
    val sink   = cpg.method("flow").ast.isReturn
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(List[(String, Option[Integer])](("z = a", 3), ("b = z", 4), ("return b;", 6)))
  }
}

class DataFlowTest18 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      | int nested(int a){
      |   int x;
      |   int z = 0x37;
      |   if(a < 10){
      |     if( a < 5){
      |       if(a < 2){
      |          x = a;
      |       }
      |     }
      |   } else
      |     x = z;
      |
      |   return x;
      | }
      """.stripMargin

  "flow with nested if-statements from method return to a" in {
    val source = cpg.call.code("a < 10").argument.code("a")
    val sink   = cpg.method("nested").ast.isReturn
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(
        List[(String, Option[Integer])](
          ("a < 10", Some(5)),
          ("a < 5", Some(6)),
          ("a < 2", Some(7)),
          ("x = a", 8),
          ("return x;", 14)
        )
      )
  }
}

class DataFlowTest19 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      | int nested(int a){
      |   int x;
      |   int z = 0x37;
      |   if(a < 10){
      |     if( a < 5){
      |       if(a < 2){
      |          x = a;
      |       }
      |     }
      |   } else
      |     x = z;
      |
      |   return x;
      | }
      """.stripMargin

  "flow with nested if-statements to `return x`" in {
    val source = cpg.identifier.name("x")
    val sink   = cpg.method("nested").ast.isReturn
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(
        List[(String, Option[Integer])](("x = z", 12), ("return x;", 14)),
        List[(String, Option[Integer])](("x = a", 8), ("return x;", 14)),
        List[(String, Option[Integer])](("return x;", 14))
      )
  }
}

class DataFlowTest20 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      | void param(int x){
      |    int a = x;
      |    int b = a;
      |    int z = foo(b);
      |  }
      """.stripMargin

  "flow chain from function argument of foo to a" in {
    implicit val callResolver: NoResolve.type = NoResolve
    val source                                = cpg.identifier.name("a")
    val sink                                  = cpg.method.name("foo").parameter.argument
    val flows                                 = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(
        List[(String, Option[Integer])](("a = x", 3), ("b = a", 4), ("foo(b)", 5)),
        List[(String, Option[Integer])](("b = a", 4), ("foo(b)", 5))
      )

  }
}

class DataFlowTest21 extends DataFlowCodeToCpgSuite {

  override val code: String = """
                                | void param(int x){
                                |    int a = x;
                                |    int b = a;
                                |    int z = foo(b);
                                |  }
      """.stripMargin

  "flow from function foo to a" in {
    val source = cpg.identifier.name("a")
    val sink   = cpg.call.name("foo").argument(1)
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(
        List[(String, Option[Integer])](("a = x", 3), ("b = a", 4), ("foo(b)", 5)),
        List[(String, Option[Integer])](("b = a", 4), ("foo(b)", 5))
      )
  }
}

class DataFlowTest22 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      | struct node {
      | int value1;
      | int value2;
      |};
      |
      |void test(void){
      |  int x = 10;
      |  struct node n;
      |  n.value1 = x;
      |  n.value2 = n.value1;
      |}
      """.stripMargin

  "flow with member access in expression to identifier x" in {
    val source = cpg.identifier.name("x")
    val sink   = cpg.call.code("n.value2")
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(
        List[(String, Option[Integer])](("x = 10", 8), ("n.value1 = x", 10), ("n.value2 = n.value1", 11)),
        List[(String, Option[Integer])](("n.value1 = x", 10), ("n.value2 = n.value1", 11))
      )
  }
}

class DataFlowTest23 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      | void flow(void) {
      |   int a = 0x37;
      |   int b=a;
      |   int c=0x31;
      |   int z = b + c;
      |   z++;
      |   int* p = &z;
      |   int x = z;
      | }
      """.stripMargin

  "flow chain from x to literal 0x37" in {
    val source = cpg.literal.code("0x37")
    val sink   = cpg.identifier.name("x")
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(
        List[(String, Option[Integer])](
          ("a = 0x37", 3),
          ("b=a", 4),
          ("b + c", 6),
          ("z = b + c", 6),
          ("z++", Some(7)),
          ("x = z", 9)
        )
      )
  }
}

class DataFlowTest24 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      | void flow(void) {
      |    int a = 0x37;
      |    int b = a;
      |    int z = b;
      |    z+=a;
      | }
       """.stripMargin

  "flow with short hand assignment operator" in {
    val source = cpg.call.code("a = 0x37").argument(2)
    val sink   = cpg.call.code("z\\+=a").argument(1)
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(List[(String, Option[Integer])](("a = 0x37", 3), ("b = a", 4), ("z = b", 5), ("z+=a", 6)))
  }
}

class DataFlowTest25 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      | void flow(void) {
      |    int a = 0x37;
      |    int b = a;
      |    int z = b;
      |    z+=a;
      |    int w = z;
      | }
      """.stripMargin

  "flow after short hand assignment" in {
    val source = cpg.call.code("a = 0x37").argument(1)
    val sink   = cpg.identifier.name("w")
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(List[(String, Option[Integer])](("a = 0x37", 3), ("b = a", 4), ("z = b", 5), ("z+=a", 6), ("w = z", 7)))
  }
}

class DataFlowTest26 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      | int main(int argc, char** argv){
      |    int x = argv[1];
      |    int y = x;
      |    int z = y;
      |
      |    return 0;
      | }
      """.stripMargin

  "flow from array method parameter to identifier" in {
    val source = cpg.method.parameter
    val sink   = cpg.identifier.name("y")
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(
        List[(String, Option[Integer])](
          ("main(int argc, char** argv)", 2),
          ("x = argv[1]", 3),
          ("y = x", 4),
          ("z = y", 5)
        ),
        List[(String, Option[Integer])](("main(int argc, char** argv)", 2), ("x = argv[1]", 3), ("y = x", 4))
      )
  }
}

class DataFlowTest27 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
  void foo(bool x, void* y) {
    void* z =  x ? f(y) : g(y);
    return;
  }
      """.stripMargin

  "conditional expressions (joern issue #91)" in {
    val source = cpg.method.parameter.name("y")
    val sink   = cpg.identifier.name("z")
    val flows  = sink.reachableByFlows(source)
    flows.size shouldBe 1
  }
}

class DataFlowTest28 extends DataFlowCodeToCpgSuite {

  override val code: String = """

  int bar() {
    int x = source();
    foo(x);
  }

  void foo(int y) {
    sink(y);
  }

  """.stripMargin

  "find source in caller" in {
    val source = cpg.call("source")
    val sink   = cpg.call("sink").argument(1)
    sink.reachableByFlows(source).map(flowToResultPairs).toSetMutable shouldBe Set(
      List(
        ("source()", Some(4)),
        ("x = source()", Some(4)),
        ("foo(x)", Some(5)),
        ("foo(int y)", Some(8)),
        ("sink(y)", Some(9))
      )
    )
  }
}

class DataFlowTest29 extends DataFlowCodeToCpgSuite {

  override val code: String = """

  int bar() {
    return source();
  }

  void foo(int y) {
    int y = bar();
    sink(y);
  }

  """.stripMargin

  "find source in callee" in {
    val source = cpg.call("source")
    val sink   = cpg.call("sink").argument(1)
    sink.reachableByFlows(source).map(flowToResultPairs).toSetMutable shouldBe Set(
      List(
        ("source()", Some(4)),
        ("return source();", Some(4)),
        ("int", Some(3)),
        ("bar()", Some(8)),
        ("y = bar()", Some(8)),
        ("sink(y)", Some(9))
      )
    )
  }

  "allow using formal parameters as sink" in {
    val source = cpg.call("source")
    val sink   = cpg.method("sink").parameter.index(1)
    sink.reachableByFlows(source).map(flowToResultPairs).toSetMutable shouldBe Set(
      List(
        ("source()", Some(4)),
        ("return source();", Some(4)),
        ("int", Some(3)),
        ("bar()", Some(8)),
        ("y = bar()", Some(8)),
        ("sink(y)", Some(9)),
        ("sink(p1)", None)
      )
    )
  }
}

class DataFlowTest30 extends DataFlowCodeToCpgSuite {

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
      |""".stripMargin

  "struct data flow" in {
    val source = cpg.method.name("source").methodReturn
    val sink   = cpg.method.name("sink").parameter.name("x")
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe Set(
      List(
        ("double", Some(7)),
        ("source(2)", Some(16)),
        ("k = source(2)", Some(16)),
        ("point.x = k", Some(18)),
        ("sink(point.x)", Some(20)),
        ("sink(int x)", Some(11))
      )
    )

  }
}

class DataFlowTest31 extends DataFlowCodeToCpgSuite {

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
      |""".stripMargin

  "tainted struct" in {
    val source = cpg.method.name("source").methodReturn
    val sink   = cpg.method.name("sink").parameter.name("x")
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe Set(
      List(
        ("struct Point", Some(7)),
        ("source(2)", Some(17)),
        ("point = source(2)", Some(17)),
        ("sink(point.x)", Some(18)),
        ("sink(int x)", Some(12))
      )
    )
  }
}

class DataFlowTest32 extends DataFlowCodeToCpgSuite {

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
      |""".stripMargin

  "should not find any flows" in {
    val source = cpg.method.name("source").methodReturn
    val sink   = cpg.method.name("sink").parameter
    val flows  = sink.reachableByFlows(source)
    flows.size shouldBe 0
  }
}

class DataFlowTest33 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      |
      | int source();
      | void sink(int* cont);
      |
      | void foo(int** c, int idx) {
      |   c[1][2] = source();
      |   c[idx][2] = 0;
      |   sink(c[1]);
      | }
      |""".stripMargin

  "should find flow" in {
    val source = cpg.method.name("source").methodReturn
    val sink   = cpg.method.name("sink").parameter
    val flows  = sink.reachableByFlows(source)

    flows.size shouldBe 1
  }
}

class DataFlowTest34 extends DataFlowCodeToCpgSuite {

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
      |""".stripMargin

  "find flows (pointer-to-struct/arrows vs star-dot)" in {
    val source = cpg.method.name("source").methodReturn
    val sink   = cpg.method.name("sink").parameter
    val flows  = sink.reachableByFlows(source)
    flows.size shouldBe 1
  }

}

class DataFlowTest35 extends DataFlowCodeToCpgSuite {

  override val code: String = """
                                | int source();
                                | void sink(int i);
                                |
                                | void foo(int* arg) {
                                |   arg[0] = source();
                                |   sink(*arg);
                                | }
                                |""".stripMargin

  "handle deref vs array access correctly" in {
    val source = cpg.method.name("source").methodReturn
    val sink   = cpg.call.codeExact("*arg")
    val flows  = sink.reachableByFlows(source)
    flows.size shouldBe 1
  }

}

class DataFlowTest36 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      |int foo() {
      |  source(&a->c);
      |  sink(a->b);
      |}
      |""".stripMargin

  "should not report flow if access path differs" in {
    val source = cpg.call.name("source").argument
    val sink   = cpg.method.name("sink").parameter
    val flows  = sink.reachableByFlows(source)
    flows.size shouldBe 0
  }
}

class DataFlowTest37 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      |int bar() {
      |  source(&a->b);
      |  sink(a->b);
      |}
      |
      |""".stripMargin

  "should report flow if address passed to source" in {
    val source = cpg.call("source").argument
    val sink   = cpg.method.name("sink").parameter
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe Set(
      List(("source(&a->b)", Some(3)), ("sink(a->b)", Some(4)), ("sink(p1)", None))
    )
  }
}

class DataFlowTest38 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      |int foo() {
      |  a->b = source();
      |  a->b = 10;
      |  sink(a->b);
      |}
      |
      |""".stripMargin

  "should not report flow" in {
    val source = cpg.call.name("source")
    val sink   = cpg.method.name("sink").parameter
    val flows  = sink.reachableByFlows(source)

    flows.size shouldBe 0

    val source2 = cpg.assignment.codeExact("a->b = 10").target
    val sink2   = cpg.method.name("sink").parameter

    sink2.reachableBy(source2).size shouldBe 1
  }
}

class DataFlowTest40 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      |int foo() {
      |   int y = 1;
      |   y = something_else;
      |   y = 10;
      |}
      |
      |""".stripMargin

  "find that there is no flow from `y = 1` to exit node" in {
    val source = cpg.literal("1")
    val sink   = cpg.method("foo").methodReturn

    val flows = sink.reachableByFlows(source)
    flows.size shouldBe 0
  }
}

class DataFlowTest41 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      |int foo() {
      |   char * y = malloc(10);
      |   free(y);
      |   y = 10;
      |}
      |
      |""".stripMargin

  "find that there is no flow from free(y) to exit node" in {
    val source = cpg.call("free").argument(1)
    val sink   = cpg.method("foo").methodReturn
    val flows  = sink.reachableByFlows(source)

    flows.size shouldBe 0
  }
}

class DataFlowTest42 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      |int foo(int b) {
      |  b = source();
      |  b = 10;
      |  sink(b);
      |}
      |
      |""".stripMargin

  "should block flow even if variable decl cannot be found" in {
    val source = cpg.call.name("source")
    val sink   = cpg.method.name("sink").parameter
    val flows  = sink.reachableByFlows(source)
    flows.size shouldBe 0

    val source2 = cpg.assignment.codeExact("b = 10").target
    val sink2   = cpg.method.name("sink").parameter

    sink2.reachableBy(source2).size shouldBe 1
  }
}

class DataFlowTest43 extends DataFlowCodeToCpgSuite {

  override val code =
    """
      int foo() {
         return bar();
      }
    """

  "should not create edges from call to ret twice" in {
    cpg
      .call("bar")
      .outE(EdgeTypes.REACHING_DEF)
      .count(_.inNode() == cpg.ret.head) shouldBe 1
  }
}

class DataFlowTest44 extends DataFlowCodeToCpgSuite {

  override val code =
    """
       void f(int x, int y)
        {
          g(x, y);
        }
    """

  "should find flow from outer params to inner params" in {
    def source = cpg.method.name("f").parameter
    def sink   = cpg.method.name("g").parameter
    sink.size shouldBe 2
    source.size shouldBe 2
    sink.reachableBy(source).size shouldBe 4
  }
}

class DataFlowTest45 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      |int bar(int z) {
      |  int x = 10;
      |  int y = x + source()
      |  return y;
      |}
      |
      |int foo() {
      |int y = bar(x);
      |sink(y);
      |}
      |""".stripMargin

  "should provide correct flow for source in sibling callee" in {
    cpg.call("sink").argument(1).reachableByFlows(cpg.call("source")).size shouldBe 1
  }

}

class DataFlowTest46 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      | void foo() {
      |   int x = source();
      |   sink(x);
      | }
      |""".stripMargin

  "should find flow via assignment" in {
    val source = cpg.call("source")
    val sink   = cpg.call("sink")
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe Set(
      List(("source()", Some(3)), ("x = source()", Some(3)), ("sink(x)", Some(4)))
    )
  }
}

class DataFlowTest47 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      | int sink(int arg){  return arg; };
      | int source(){ return 0; };
      |
      | void foo() {
      |   sink(source());
      | }
      |""".stripMargin

  "should find flow of call in call" in {
    val source = cpg.call("source")
    val sink   = cpg.call("sink")
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe Set(List(("source()", Some(6)), ("sink(source())", Some(6))))
  }
}

class DataFlowTest49 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      | void foo(int x) {
      |   x = source();
      |   sink(x);
      | }
      |""".stripMargin

  "should find flow via assignment for global" in {
    val source = cpg.call("source")
    val sink   = cpg.call("sink")
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe Set(
      List(("source()", Some(3)), ("x = source()", Some(3)), ("sink(x)", Some(4)))
    )
  }
}

class DataFlowTest50 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      | void foo() {
      |   int x = source();
      |   x = y;
      |   sink(x);
      | }
      |""".stripMargin

  "should find that flow is blocked by assignment" in {
    val source     = cpg.call("source")
    val assignment = cpg.assignment.codeExact("x = y")
    val sink       = cpg.call("sink").l

    val flows = sink.reachableByFlows(source)
    flows.size shouldBe 0

    val flows2 = sink.reachableByFlows(assignment.target)
    flows2.map(flowToResultPairs).toSetMutable shouldBe Set(List(("x = y", Some(4)), ("sink(x)", Some(5))))
  }
}

class DataFlowTest51 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      | void foo() {
      |   x.y = source();
      |   sink(x.y);
      | }
      |""".stripMargin

  "should find via assignment with field access" in {
    val source = cpg.call("source")
    val sink   = cpg.call("sink")
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe Set(
      List(("source()", Some(3)), ("x.y = source()", Some(3)), ("sink(x.y)", Some(4)))
    )
  }
}

class DataFlowTest52 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      | void foo() {
      |   x->y = source();
      |   sink(x->y);
      | }
      |""".stripMargin

  "should find flow via assignment with indirect field access" in {
    val source = cpg.call("source")
    val sink   = cpg.call("sink")
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe Set(
      List(("source()", Some(3)), ("x->y = source()", Some(3)), ("sink(x->y)", Some(4)))
    )
  }
}

class DataFlowTest53 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      | void foo() {
      |   int x.y = source();
      |   x.y = z;
      |   sink(x);
      | }
      |""".stripMargin

  "should find that flow is blocked by assignment" in {
    def source     = cpg.call("source")
    def sink       = cpg.call("sink")
    val assignment = cpg.assignment.codeExact("x.y = z")

    val flows = sink.reachableByFlows(source)
    flows.size shouldBe 0

    val flows2 = sink.reachableByFlows(assignment.target)
    flows2.map(flowToResultPairs).toSetMutable shouldBe Set(List(("x.y = z", Some(4)), ("sink(x)", Some(5))))
  }
}

class DataFlowTest54 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      |void foo() {
      |  int data;
      |  fscanf(stdin, "%d", &data);
      |  int result = data + 1;
      |  printf("%d\n", result);
      |}
      |""".stripMargin

  "should find flow via fscanf" in {
    def source = cpg.call("fscanf").argument

    def sink = cpg.identifier("result")

    def flows = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe Set(
      List(
        ("fscanf(stdin, \"%d\", &data)", Some(4)),
        ("data + 1", Some(5)),
        ("result = data + 1", Some(5)),
        ("printf(\"%d\\n\", result)", Some(6))
      ),
      List(("fscanf(stdin, \"%d\", &data)", Some(4)), ("data + 1", Some(5)), ("result = data + 1", Some(5)))
    )
  }
}

class DataFlowTest55 extends DataFlowCodeToCpgSuite {

  override val code: String = """
    | struct node {
    |  int value;
    |  struct node *next;
    | };
    |
    | void free_list(struct node *head) {
    |  struct node *q;
    |  for (struct node *p = head; p != NULL; p = q) {
    |    q = p->next;
    |    free(p);
    |  }
    | }
    | 
    | int flow(int p0) {
    |  int a = p0;
    |  int b=a;
    |  int c=0x31;
    |  int z = b + c;
    |  z++;
    |  int x = z;
    |  return x;
    | }""".stripMargin

  "should identify all calls to `free`" in {
    cpg.call.name("free").code.toSetMutable shouldBe Set("free(p)")
  }

  "should find flows to arguments of `free`" in {
    implicit val callResolver: NoResolve.type = NoResolve
    val source                                = cpg.identifier
    val sink                                  = cpg.method.name("free").parameter.argument
    sink.reachableByFlows(source).l.map(flowToResultPairs).distinct.toSet.size shouldBe 5
  }

  "should find flows to `free`" in {
    val source = cpg.identifier
    val sink   = cpg.call.name("free")
    sink.reachableByFlows(source).l.map(flowToResultPairs).distinct.toSet.size shouldBe 5
  }

  "should find flows from identifiers to return values of `flow`" in {
    val source = cpg.identifier
    val sink   = cpg.method.name("flow").methodReturn
    sink.reachableByFlows(source).l.map(flowToResultPairs).distinct.toSet.size shouldBe 9
  }

  "find flows from z to method returns of flow" in {
    val source = cpg.identifier.name("z")
    val sink   = cpg.method.name("flow").methodReturn
    sink.reachableByFlows(source).l.size shouldBe 3
  }

}

class DataFlowTest56 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      |int test() {
      |  char inputBuffer[0x100] = "";
      |  int buffer[10] = {0};
      |  int data = 1;     
      |  fgets(inputBuffer, 0x100, stdin);
      |  data = atoi(inputBuffer);
      |  buffer[data] = 1;
      |  strncpy(buffer, "hello", data);
      |  return 0;
      |}""".stripMargin

  "should find flow from <operator>.indirectIndexAccess" in {

    def source = cpg.call("fgets").argument(1)

    def sink1 = cpg.call("strncpy").argument(3)

    def sink2 = cpg.call("<operator>.indirectIndexAccess").argument(2)

    def flows1 = sink1.reachableByFlows(source)

    flows1.map(flowToResultPairs).toSetMutable shouldBe Set(
      List(
        ("fgets(inputBuffer, 0x100, stdin)", Some(6)),
        ("atoi(inputBuffer)", Some(7)),
        ("data = atoi(inputBuffer)", Some(7)),
        ("strncpy(buffer, \"hello\", data)", Some(9))
      )
    )

    def flows2 = sink2.reachableByFlows(source)

    flows2.map(flowToResultPairs).toSetMutable shouldBe Set(
      List(
        ("fgets(inputBuffer, 0x100, stdin)", Some(6)),
        ("atoi(inputBuffer)", Some(7)),
        ("data = atoi(inputBuffer)", Some(7)),
        ("buffer[data] = 1", Some(8))
      )
    )
  }
}

class DataFlowTest57 extends DataFlowCodeToCpgSuite {
  override val code: String =
    """
      |void abc()
      |{
      |    int a;
      |    a = foo();
      |    a = bar(0x80);
      |    sink(a);
      |}
      |""".stripMargin

  "should not find a flow from 'a' at 'foo' to 'sink'" in {
    def src = cpg.call("foo").inAssignment.target.head
    def snk = cpg.method("sink").parameter
    snk.reachableByFlows(src).size shouldBe 0
  }
}

class DataFlowTest58 extends DataFlowCodeToCpgSuite {
  override val code: String =
    """
      |void abc(int a)
      |{
      |    a = foo();
      |    a = bar(0x80);
      |    sink(a);
      |}
      |""".stripMargin

  "should not find a flow from parameter 'a' to 'sink'" in {
    def src = cpg.method("abc").parameter
    def snk = cpg.method("sink").parameter
    snk.reachableByFlows(src).size shouldBe 0
  }
}

class DataFlowTest59 extends DataFlowCodeToCpgSuite {
  override val code: String =
    """
      |typedef struct {
      | char *buf1;
      |} FooStruct;
      |
      |void doFoo(FooStruct *str) {
      |}
      |int main(void) {
      | FooStruct foo;
      | doFoo(&foo);
      | return 0;
      |}
      |""".stripMargin

  "should find flow from local to 'doFoo'" in {
    def source = cpg.local.name("foo").referencingIdentifiers
    def sink   = cpg.call.code("doFoo.*").argument
    sink.reachableByFlows(source).size shouldBe 1
  }

}

class DataFlowTests60 extends DataFlowCodeToCpgSuite {

  override val code: String = """
      |
      |void outer(char* ptr){
      | taint1(ptr);
      | inner(ptr);
      | return;
      | }
      | void inner(char * ptr)
      | {
      | // taint2(ptr);
      | ptr = malloc(0x80);
      | sink(ptr);
      | }""".stripMargin

  "should not return flow" in {
    def source = cpg.call("taint1").argument
    def sink   = cpg.call("sink").argument
    sink.reachableByFlows(source).size shouldBe 0
  }

}

class DataFlowTests61 extends DataFlowCodeToCpgSuite {
  override val code: String =
    """
      |void reassignThenFree(char * ptr)
      |{
      |ptr = malloc(0x80);
      |free(ptr);
      |return;
      |}
      |
      |void reassign(char * ptr)
      |{
      |ptr = malloc(0x80);
      |return;
      |}
      |
      |// This flow from `free` to `free` should be returned
      |int case0()
      |{
      |char * data = malloc(0x100);
      |free(data);
      |free(data);
      |return 0;
      |}
      |
      |""".stripMargin

  "should find flow from `free` to `free`" in {
    def sink   = cpg.call("free").argument(1).l
    def source = cpg.call("free").argument(1).l
    val List(flow: Path) = sink
      .reachableByFlows(source)
      .filter(path => path.elements.size > 1)
      .l
    flow.elements match {
      case List(i1: Identifier, i2: Identifier) =>
        i1.name shouldBe "data"
        i1.lineNumber shouldBe Some(19)
        i2.name shouldBe "data"
        i2.lineNumber shouldBe Some(20)
    }
  }
}

class DataFlowTests62 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      |void reassignThenFree(char * ptr)
      |{
      |ptr = malloc(0x80);
      |free(ptr);
      |return;
      |}
      |
      |void reassign(char * ptr)
      |{
      |ptr = malloc(0x80);
      |return;
      |}
      |
      |// This flow should NOT be returned
      |int case1()
      |{
      |char * data = malloc(0x100);
      |free(data);
      |data = malloc(0x80);
      |free(data);
      |return 0;
      |}
      |
      |""".stripMargin

  "should not report flow" in {
    def sink   = cpg.call("free").argument(1).l
    def source = cpg.call("free").argument(1).l
    sink.reachableByFlows(source).count(path => path.elements.size > 1) shouldBe 0
  }
}

class DataFlowTests63 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      |void reassignThenFree(char * ptr)
      |{
      |ptr = malloc(0x80);
      |free(ptr);
      |return;
      |}
      |
      |// This flow should NOT be returned
      |int case2()
      |{
      |char * data = malloc(0x100);
      |free(data);
      |reassignThenFree(data);
      |return 0;
      |}
      |
      |""".stripMargin

  "should not report flow" in {
    def sink   = cpg.call("free").argument(1).l
    def source = cpg.call("free").argument(1).l
    sink.reachableByFlows(source).count(path => path.elements.size > 1) shouldBe 0
  }
}

class DataFlowTests64 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      |void reassign(char * ptr)
      |{
      |ptr = malloc(0x80);
      |return;
      |}
      |
      |// This flow should NOT be returned
      |int case3()
      |{
      |char * data = malloc(0x100);
      |free(data);
      |reassign(data);
      |free(data);
      |return 0;
      |}
      |""".stripMargin

  "should not report flow" in {
    def sink   = cpg.call("free").argument(1).l
    def source = cpg.call("free").argument(1).l
    sink.reachableByFlows(source).count(path => path.elements.size > 1) shouldBe 0
  }

}

class DataFlowTests56 extends DataFlowCodeToCpgSuite {

  override val code: String = """
  int foo(int x) {
    x = 10;
  }
  """

  "should report flow from method assignment to method parameter out" in {
    def sink   = cpg.method("foo").parameter.asOutput.l
    def source = cpg.method("foo").ast.isIdentifier.name("x")
    sink.reachableByFlows(source).size shouldBe 1
  }

}
