package io.joern.c2cpg.querying

import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.Identifier
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.toNodeTraversal

class DataFlowTests extends DataFlowCodeToCpgSuite {

  "DataFlowTest1" should {
    val cpg = code("""
        |struct node {
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
        | }""".stripMargin)

    "identify all calls to `free`" in {
      cpg.call.name("free").code.toSetMutable shouldBe Set("free(p)")
    }

    "find flows to arguments of `free`" in {
      implicit val callResolver: NoResolve.type = NoResolve
      val source                                = cpg.identifier
      val sink                                  = cpg.method.name("free").parameter.argument
      sink.reachableByFlows(source).l.map(flowToResultPairs).distinct.size shouldBe 5
    }

    "find flows to `free`" in {
      val source = cpg.identifier
      val sink   = cpg.call.name("free")
      sink.reachableByFlows(source).l.map(flowToResultPairs).distinct.size shouldBe 5
    }

    "find flows from identifiers to return values of `flow`" in {
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

  "DataFlowTest2" should {

    val cpg = code("""
        | int main(int x) {
        |  return x;
        |}
        """.stripMargin)

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

  "DataFlowTest3" should {

    val cpg = code("""
        | int main(int x) {
        |  int k = x + 1;
        |  int y = k + 2;
        |  return y + 3;
        |}
          """.stripMargin)

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

  "DataFlowTest4" should {

    val cpg = code("""
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
          """.stripMargin)

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

  "DataFlowTest5" should {

    val cpg = code("""
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
        | }""".stripMargin)

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

  "DataFlowTest6" should {
    val cpg = code("""
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
          """.stripMargin)

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

  "DataFlowTest7" should {
    val cpg = code("""
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
          """.stripMargin)

    "Overtaint behind exclusion" should {
      "not find any flows" in {
        val source = cpg.method.name("source").methodReturn
        val sink   = cpg.method.name("sink").parameter
        val flows  = sink.reachableByFlows(source)

        flows.size shouldBe 0
      }
    }

  }

  "DataFlowTest8" should {
    val cpg = code("""
        | int source();
        | void sink(int* cont);
        |
        | void foo(int** c, int idx) {
        |   c[1][2] = source();
        |   c[idx][2] = 0;
        |   sink(c[1]);
        | }
          """.stripMargin)

    "Exclusions behind over-taint" should {
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

  "DataFlowTest9" should {
    val cpg = code("""
        |typedef struct {int field;} S;
        | int source();
        | void sink(int i);
        |
        | void foo(S* arg) {
        |   arg->field = source();
        |   sink((*arg).field);
        | }
          """.stripMargin)

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

  "DataFlowTest10" should {
    val cpg = code("""
        | int source();
        | void sink(int i);
        |
        | void foo(int* arg) {
        |   arg[0] = source();
        |   sink(*arg);
        | }
          """.stripMargin)

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

  "DataFlowTest11" should {
    val cpg = code("""
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
        |}""".stripMargin)

    "PathUnfolding with allFlows" should {
      // regression test for  https://github.com/ShiftLeftSecurity/product/issues/7017
      "work as expected" in {
        def source = cpg.call("getpid")

        def sink = cpg.ret

        val flows = sink.reachableByFlows(source)

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

  "DataFlowTest12" should {
    val cpg = code("""
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
          """.stripMargin)

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

        pendingUntilFixed({
          flows.size shouldBe 0
        })

      }
    }

  }

  "DataFlowTest13" should {
    val cpg = code("""
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
      """.stripMargin)

    "flow from function call read to multiple versions of the same variable" in {
      def source = cpg.identifier.name("sz")

      def sink = cpg.call.name("read")

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

  "DataFlowTest14" should {
    val cpg = code("""
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
      """.stripMargin)

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

  "DataFlowTest15" should {
    val cpg = code("""
        | int method(int y){
        |  int a = 10;
        |  if (a < y){
        |    foo(a);
        |  }
        | }
      """.stripMargin)

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

  "DataFlowTest16" should {
    val cpg = code("""
        | void flow(void) {
        |   int a = 0x37;
        |   int b=a;
        |   int c=0x31;
        |   int z = b + c;
        |   z++;
        |   int* p = &z;
        |   int x = z;
        | }
      """.stripMargin)

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

  "DataFlowTest17" should {
    val cpg = code("""
        | int flow(int a){
        |   int z = a;
        |   int b = z;
        |
        |   return b;
        | }
      """.stripMargin)

    "flow from method return to a" in {
      val source = cpg.identifier.name("a")
      val sink   = cpg.method("flow").ast.isReturn
      val flows  = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSetMutable shouldBe
        Set(List[(String, Option[Integer])](("z = a", 3), ("b = z", 4), ("return b;", 6)))
    }
  }

  "DataFlowTest18" should {
    val cpg = code("""
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
      """.stripMargin)

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

  "DataFlowTest19" should {
    val cpg = code("""
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
      """.stripMargin)

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

  "DataFlowTest20" should {
    val cpg = code("""
        | void param(int x){
        |    int a = x;
        |    int b = a;
        |    int z = foo(b);
        |  }
      """.stripMargin)

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

  "DataFlowTest21" should {
    val cpg = code("""
        | void param(int x){
        |    int a = x;
        |    int b = a;
        |    int z = foo(b);
        |  }
      """.stripMargin)

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

  "DataFlowTest22" should {
    val cpg = code("""
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
      """.stripMargin)

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

  "DataFlowTest23" should {
    val cpg = code("""
        | void flow(void) {
        |   int a = 0x37;
        |   int b=a;
        |   int c=0x31;
        |   int z = b + c;
        |   z++;
        |   int* p = &z;
        |   int x = z;
        | }
      """.stripMargin)

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

  "DataFlowTest24" should {
    val cpg = code("""
        | void flow(void) {
        |    int a = 0x37;
        |    int b = a;
        |    int z = b;
        |    z+=a;
        | }
       """.stripMargin)

    "flow with short hand assignment operator" in {
      val source = cpg.call.code("a = 0x37").argument(2)
      val sink   = cpg.call.code("z\\+=a").argument(1)
      val flows  = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSetMutable shouldBe
        Set(List[(String, Option[Integer])](("a = 0x37", 3), ("b = a", 4), ("z = b", 5), ("z+=a", 6)))
    }
  }

  "DataFlowTest25" should {
    val cpg = code("""
        | void flow(void) {
        |    int a = 0x37;
        |    int b = a;
        |    int z = b;
        |    z+=a;
        |    int w = z;
        | }
      """.stripMargin)

    "flow after short hand assignment" in {
      val source = cpg.call.code("a = 0x37").argument(1)
      val sink   = cpg.identifier.name("w")
      val flows  = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSetMutable shouldBe
        Set(List[(String, Option[Integer])](("a = 0x37", 3), ("b = a", 4), ("z = b", 5), ("z+=a", 6), ("w = z", 7)))
    }
  }

  "DataFlowTest26" should {
    val cpg = code("""
        | int main(int argc, char** argv){
        |    int x = argv[1];
        |    int y = x;
        |    int z = y;
        |
        |    return 0;
        | }
      """.stripMargin)

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

  "DataFlowTest27" should {
    val cpg = code("""
  void foo(bool x, void* y) {
    void* z =  x ? f(y) : g(y);
    return;
  }
      """.stripMargin)

    "conditional expressions (joern issue #91)" in {
      val source = cpg.method.parameter.name("y")
      val sink   = cpg.identifier.name("z")
      val flows  = sink.reachableByFlows(source)
      flows.size shouldBe 1
    }
  }

  "DataFlowTest28" should {
    val cpg = code("""

  int bar() {
    int x = source();
    foo(x);
  }

  void foo(int y) {
    sink(y);
  }

  """.stripMargin)

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

  "DataFlowTest29" should {
    val cpg = code("""

  int bar() {
    return source();
  }

  void foo(int y) {
    int y = bar();
    sink(y);
  }

  """.stripMargin)

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

  "DataFlowTest30" should {
    val cpg = code("""
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
        |""".stripMargin)

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

  "DataFlowTest31" should {
    val cpg = code("""
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
        |""".stripMargin)

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

  "DataFlowTest32" should {
    val cpg = code("""
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
        |""".stripMargin)

    "not find any flows" in {
      val source = cpg.method.name("source").methodReturn
      val sink   = cpg.method.name("sink").parameter
      val flows  = sink.reachableByFlows(source)
      flows.size shouldBe 0
    }
  }

  "DataFlowTest33" should {
    val cpg = code("""
        |
        | int source();
        | void sink(int* cont);
        |
        | void foo(int** c, int idx) {
        |   c[1][2] = source();
        |   c[idx][2] = 0;
        |   sink(c[1]);
        | }
        |""".stripMargin)

    "find flow" in {
      val source = cpg.method.name("source").methodReturn
      val sink   = cpg.method.name("sink").parameter
      val flows  = sink.reachableByFlows(source)

      flows.size shouldBe 1
    }
  }

  "DataFlowTest34" should {
    val cpg = code("""
        |typedef struct {int field;} S;
        | int source();
        | void sink(int i);
        |
        | void foo(S* arg) {
        |   arg->field = source();
        |   sink((*arg).field);
        | }
        |""".stripMargin)

    "find flows (pointer-to-struct/arrows vs star-dot)" in {
      val source = cpg.method.name("source").methodReturn
      val sink   = cpg.method.name("sink").parameter
      val flows  = sink.reachableByFlows(source)
      flows.size shouldBe 1
    }

  }

  "DataFlowTest35" should {
    val cpg = code("""
        | int source();
        | void sink(int i);
        |
        | void foo(int* arg) {
        |   arg[0] = source();
        |   sink(*arg);
        | }
        |""".stripMargin)

    "handle deref vs array access correctly" in {
      val source = cpg.method.name("source").methodReturn
      val sink   = cpg.call.codeExact("*arg")
      val flows  = sink.reachableByFlows(source)
      flows.size shouldBe 1
    }

  }

  "DataFlowTest36" should {
    val cpg = code("""
        |int foo() {
        |  source(&a->c);
        |  sink(a->b);
        |}
        |""".stripMargin)

    "not report flow if access path differs" in {
      val source = cpg.call.name("source").argument
      val sink   = cpg.method.name("sink").parameter
      val flows  = sink.reachableByFlows(source)
      flows.size shouldBe 0
    }
  }

  "DataFlowTest37" should {
    val cpg = code("""
        |int bar() {
        |  source(&a->b);
        |  sink(a->b);
        |}
        |
        |""".stripMargin)

    "report flow if address passed to source" in {
      val source = cpg.call("source").argument
      val sink   = cpg.method.name("sink").parameter
      val flows  = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSetMutable shouldBe Set(
        List(("source(&a->b)", Some(3)), ("sink(a->b)", Some(4)), ("sink(p1)", None))
      )
    }
  }

  "DataFlowTest38" should {
    val cpg = code("""
        |int foo() {
        |  a->b = source();
        |  a->b = 10;
        |  sink(a->b);
        |}
        |
        |""".stripMargin)

    "not report flow" in {
      val source = cpg.call.name("source")
      val sink   = cpg.method.name("sink").parameter
      val flows  = sink.reachableByFlows(source)

      flows.size shouldBe 0

      val source2 = cpg.assignment.codeExact("a->b = 10").target
      val sink2   = cpg.method.name("sink").parameter

      sink2.reachableBy(source2).size shouldBe 1
    }
  }

  "DataFlowTest40" should {
    val cpg = code("""
        |int foo() {
        |   int y = 1;
        |   y = something_else;
        |   y = 10;
        |}
        |
        |""".stripMargin)

    "find that there is no flow from `y = 1` to exit node" in {
      val source = cpg.literal("1")
      val sink   = cpg.method("foo").methodReturn

      val flows = sink.reachableByFlows(source)
      flows.size shouldBe 0
    }
  }

  "DataFlowTest41" should {
    val cpg = code("""
        |int foo() {
        |   char * y = malloc(10);
        |   free(y);
        |   y = 10;
        |}
        |
        |""".stripMargin)

    "find that there is no flow from free(y) to exit node" in {
      val source = cpg.call("free").argument(1)
      val sink   = cpg.method("foo").methodReturn
      val flows  = sink.reachableByFlows(source)

      flows.size shouldBe 0
    }
  }

  "DataFlowTest42" should {
    val cpg = code("""
        |int foo(int b) {
        |  b = source();
        |  b = 10;
        |  sink(b);
        |}
        |
        |""".stripMargin)

    "block flow even if variable decl cannot be found" in {
      val source = cpg.call.name("source")
      val sink   = cpg.method.name("sink").parameter
      val flows  = sink.reachableByFlows(source)
      flows.size shouldBe 0

      val source2 = cpg.assignment.codeExact("b = 10").target
      val sink2   = cpg.method.name("sink").parameter

      sink2.reachableBy(source2).size shouldBe 1
    }
  }

  "DataFlowTest43" should {
    val cpg = code("""
        |int foo() {
        |   return bar();
        |}
    """.stripMargin)

    "not create edges from call to ret twice" in {
      cpg
        .call("bar")
        .outE(EdgeTypes.REACHING_DEF)
        .count(_.inNode() == cpg.ret.head) shouldBe 1
    }
  }

  "DataFlowTest44" should {
    val cpg = code("""
        |void f(int x, int y)
        |{
        |  g(x, y);
        |}
    """.stripMargin)

    "find flow from outer params to inner params" in {
      def source = cpg.method.name("f").parameter

      def sink = cpg.method.name("g").parameter

      sink.size shouldBe 2
      source.size shouldBe 2
      sink.reachableBy(source).size shouldBe 4
    }
  }

  "DataFlowTest45" should {
    val cpg = code("""
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
        |""".stripMargin)

    "provide correct flow for source in sibling callee" in {
      cpg.call("sink").argument(1).reachableByFlows(cpg.call("source")).size shouldBe 1
    }

  }

  "DataFlowTest46" should {
    val cpg = code("""
        | void foo() {
        |   int x = source();
        |   sink(x);
        | }
        |""".stripMargin)

    "find flow via assignment" in {
      val source = cpg.call("source")
      val sink   = cpg.call("sink")
      val flows  = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSetMutable shouldBe Set(
        List(("source()", Some(3)), ("x = source()", Some(3)), ("sink(x)", Some(4)))
      )
    }
  }

  "DataFlowTest47" should {
    val cpg = code("""
        | int sink(int arg){  return arg; };
        | int source(){ return 0; };
        |
        | void foo() {
        |   sink(source());
        | }
        |""".stripMargin)

    "find flow of call in call" in {
      val source = cpg.call("source")
      val sink   = cpg.call("sink")
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSetMutable shouldBe Set(List(("source()", Some(6)), ("sink(source())", Some(6))))
    }
  }

  "DataFlowTest49" should {
    val cpg = code("""
        | void foo(int x) {
        |   x = source();
        |   sink(x);
        | }
        |""".stripMargin)

    "find flow via assignment for global" in {
      val source = cpg.call("source")
      val sink   = cpg.call("sink")
      val flows  = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSetMutable shouldBe Set(
        List(("source()", Some(3)), ("x = source()", Some(3)), ("sink(x)", Some(4)))
      )
    }
  }

  "DataFlowTest50" should {
    val cpg = code("""
        | void foo() {
        |   int x = source();
        |   x = y;
        |   sink(x);
        | }
        |""".stripMargin)

    "find that flow is blocked by assignment" in {
      val source     = cpg.call("source")
      val assignment = cpg.assignment.codeExact("x = y")
      val sink       = cpg.call("sink").l

      val flows = sink.reachableByFlows(source)
      flows.size shouldBe 0

      val flows2 = sink.reachableByFlows(assignment.target)
      flows2.map(flowToResultPairs).toSetMutable shouldBe Set(List(("x = y", Some(4)), ("sink(x)", Some(5))))
    }
  }

  "DataFlowTest51" should {
    val cpg = code("""
        | void foo() {
        |   x.y = source();
        |   sink(x.y);
        | }
        |""".stripMargin)

    "find via assignment with field access" in {
      val source = cpg.call("source")
      val sink   = cpg.call("sink")
      val flows  = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSetMutable shouldBe Set(
        List(("source()", Some(3)), ("x.y = source()", Some(3)), ("sink(x.y)", Some(4)))
      )
    }
  }

  "DataFlowTest52" should {
    val cpg = code("""
        | void foo() {
        |   x->y = source();
        |   sink(x->y);
        | }
        |""".stripMargin)

    "find flow via assignment with indirect field access" in {
      val source = cpg.call("source")
      val sink   = cpg.call("sink")
      val flows  = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSetMutable shouldBe Set(
        List(("source()", Some(3)), ("x->y = source()", Some(3)), ("sink(x->y)", Some(4)))
      )
    }
  }

  "DataFlowTest53" should {
    val cpg = code("""
        | void foo() {
        |   int x.y = source();
        |   x.y = z;
        |   sink(x);
        | }
        |""".stripMargin)

    "find that flow is blocked by assignment" in {
      def source = cpg.call("source")

      def sink = cpg.call("sink")

      val assignment = cpg.assignment.codeExact("x.y = z")

      val flows = sink.reachableByFlows(source)
      flows.size shouldBe 0

      val flows2 = sink.reachableByFlows(assignment.target)
      flows2.map(flowToResultPairs).toSetMutable shouldBe Set(List(("x.y = z", Some(4)), ("sink(x)", Some(5))))
    }
  }

  "DataFlowTest54" should {
    val cpg = code("""
        |void foo() {
        |  int data;
        |  fscanf(stdin, "%d", &data);
        |  int result = data + 1;
        |  printf("%d\n", result);
        |}
        |""".stripMargin)

    "find flow via fscanf" in {
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

  "DataFlowTest55" should {
    val cpg = code("""
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
        | }""".stripMargin)

    "identify all calls to `free`" in {
      cpg.call.name("free").code.toSetMutable shouldBe Set("free(p)")
    }

    "find flows to arguments of `free`" in {
      implicit val callResolver: NoResolve.type = NoResolve
      val source                                = cpg.identifier
      val sink                                  = cpg.method.name("free").parameter.argument
      sink.reachableByFlows(source).l.map(flowToResultPairs).distinct.toSet.size shouldBe 5
    }

    "find flows to `free`" in {
      val source = cpg.identifier
      val sink   = cpg.call.name("free")
      sink.reachableByFlows(source).l.map(flowToResultPairs).distinct.toSet.size shouldBe 5
    }

    "find flows from identifiers to return values of `flow`" in {
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

  "DataFlowTest56" should {
    val cpg = code("""
        |int test() {
        |  char inputBuffer[0x100] = "";
        |  int buffer[10] = {0};
        |  int data = 1;     
        |  fgets(inputBuffer, 0x100, stdin);
        |  data = atoi(inputBuffer);
        |  buffer[data] = 1;
        |  strncpy(buffer, "hello", data);
        |  return 0;
        |}""".stripMargin)

    "find flow from <operator>.indirectIndexAccess" in {

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

  "DataFlowTest57" should {
    val cpg = code("""
        |void abc()
        |{
        |    int a;
        |    a = foo();
        |    a = bar(0x80);
        |    sink(a);
        |}
        |""".stripMargin)

    "not find a flow from 'a' at 'foo' to 'sink'" in {
      def src = cpg.call("foo").inAssignment.target.head

      def snk = cpg.method("sink").parameter

      snk.reachableByFlows(src).size shouldBe 0
    }
  }

  "DataFlowTest58" should {
    val cpg = code("""
        |void abc(int a)
        |{
        |    a = foo();
        |    a = bar(0x80);
        |    sink(a);
        |}
        |""".stripMargin)

    "not find a flow from parameter 'a' to 'sink'" in {
      def src = cpg.method("abc").parameter

      def snk = cpg.method("sink").parameter

      snk.reachableByFlows(src).size shouldBe 0
    }
  }

  "DataFlowTest59" should {
    val cpg = code("""
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
        |""".stripMargin)

    "find flow from local to 'doFoo'" in {
      def source = cpg.local.name("foo").referencingIdentifiers

      def sink = cpg.call.code("doFoo.*").argument

      sink.reachableByFlows(source).size shouldBe 1
    }

  }

  "DataFlowTests60" should {
    val cpg = code("""
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
        | }""".stripMargin)

    "not return flow" in {
      def source = cpg.call("taint1").argument

      def sink = cpg.call("sink").argument

      sink.reachableByFlows(source).size shouldBe 0
    }

  }

  "DataFlowTests61" should {
    val cpg = code("""
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
        |""".stripMargin)

    "find flow from `free` to `free`" in {
      def sink = cpg.call("free").argument(1)

      def source = cpg.call("free").argument(1)

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

  "DataFlowTests62" should {
    val cpg = code("""
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
        |""".stripMargin)

    "not report flow" in {
      def sink = cpg.call("free").argument(1)

      def source = cpg.call("free").argument(1)

      sink.reachableByFlows(source).count(path => path.elements.size > 1) shouldBe 0
    }
  }

  "DataFlowTests63" should {
    val cpg = code("""
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
        |""".stripMargin)

    "not report flow" in {
      def sink = cpg.call("free").argument(1)

      def source = cpg.call("free").argument(1)

      sink.reachableByFlows(source).count(path => path.elements.size > 1) shouldBe 0
    }
  }

  "DataFlowTests64" should {
    // This case is a double-free that we return, the reason being that modifying `ptr`
    // does not modify `data` as arguments  are passed by value.
    val cpg = code("""
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
        |""".stripMargin)

    "report flow" in {
      def sink = cpg.call("free").argument(1)

      def source = cpg.call("free").argument(1)

      sink.reachableByFlows(source).count(path => path.elements.size > 1) shouldBe 1
    }

  }

  "DataFlowTests65" should {
    val cpg = code("""
        |char * reassign(char * ptr)
        |{
        |ptr = malloc(0x80);
        |return ptr;
        |}
        |
        |int case3()
        |{
        |char * data = malloc(0x80);
        |free(data);
        |data = reassign(data);
        |free(data):
        |return 0;
        |}
        |""".stripMargin)

    "not report flow from free to free" in {
      def sink = cpg.call("free").argument(1)

      def source = cpg.call("free").argument(1)

      sink.reachableByFlows(source).count(path => path.elements.size > 1) shouldBe 0
    }

  }

  "DataFlowTests66" should {
    val cpg = code("""
        |int foo(int x) {
        |  x = 10;
        |}
  """.stripMargin)

    "report flow from method assignment to method parameter out" in {
      def sink = cpg.method("foo").parameter.asOutput

      def source = cpg.method("foo").ast.isIdentifier.name("x")

      sink.reachableByFlows(source).size shouldBe 1
    }

  }
}

class DataFlowTestsWithCallDepth extends DataFlowCodeToCpgSuite {

  override def beforeAll(): Unit = {
    super.beforeAll()
    context = EngineContext(semantics, EngineConfig(maxCallDepth = -1))
  }

  "DataFlowTests67" should {
    val cpg = code("""
      |void CWE415_Double_Free__malloc_free_char_53b_badSink(char * data);
      |
      |void CWE415_Double_Free__malloc_free_char_53_bad()
      |{
      |    char * data;
      |    /* Initialize data */
      |    data = NULL;
      |    data = (char *)malloc(100*sizeof(char));
      |    if (data == NULL) {exit(-1);}
      |    /* POTENTIAL FLAW: Free data in the source - the bad sink frees data as well */
      |    free(data);
      |    CWE415_Double_Free__malloc_free_char_53b_badSink(data);
      |}
      |
      |void CWE415_Double_Free__malloc_free_char_53b_goodG2BSink(char * data);
      |
      |static void goodG2B()
      |{
      |    char * data;
      |    /* Initialize data */
      |    data = NULL;
      |    data = (char *)malloc(100*sizeof(char));
      |    if (data == NULL) {exit(-1);}
      |    /* FIX: Do NOT free data in the source - the bad sink frees data */
      |    CWE415_Double_Free__malloc_free_char_53b_goodG2BSink(data);
      |}
      |
      |void CWE415_Double_Free__malloc_free_char_53b_goodB2GSink(char * data);
      |
      |static void goodB2G()
      |{
      |    char * data;
      |    /* Initialize data */
      |    data = NULL;
      |    data = (char *)malloc(100*sizeof(char));
      |    if (data == NULL) {exit(-1);}
      |    /* POTENTIAL FLAW: Free data in the source - the bad sink frees data as well */
      |    free(data);
      |    CWE415_Double_Free__malloc_free_char_53b_goodB2GSink(data);
      |}
      |
      |void CWE415_Double_Free__malloc_free_char_53c_badSink(char * data);
      |
      |void CWE415_Double_Free__malloc_free_char_53b_badSink(char * data)
      |{
      |    CWE415_Double_Free__malloc_free_char_53c_badSink(data);
      |}
      |
      |void CWE415_Double_Free__malloc_free_char_53c_goodG2BSink(char * data);
      |
      |void CWE415_Double_Free__malloc_free_char_53b_goodG2BSink(char * data)
      |{
      |    CWE415_Double_Free__malloc_free_char_53c_goodG2BSink(data);
      |}
      |
      |void CWE415_Double_Free__malloc_free_char_53c_goodB2GSink(char * data);
      |
      |void CWE415_Double_Free__malloc_free_char_53b_goodB2GSink(char * data)
      |{
      |    CWE415_Double_Free__malloc_free_char_53c_goodB2GSink(data);
      |}
      |
      |void CWE415_Double_Free__malloc_free_char_53d_badSink(char * data);
      |
      |void CWE415_Double_Free__malloc_free_char_53c_badSink(char * data)
      |{
      |    CWE415_Double_Free__malloc_free_char_53d_badSink(data);
      |}
      |
      |void CWE415_Double_Free__malloc_free_char_53d_goodG2BSink(char * data);
      |
      |void CWE415_Double_Free__malloc_free_char_53c_goodG2BSink(char * data)
      |{
      |    CWE415_Double_Free__malloc_free_char_53d_goodG2BSink(data);
      |}
      |
      |void CWE415_Double_Free__malloc_free_char_53d_goodB2GSink(char * data);
      |
      |void CWE415_Double_Free__malloc_free_char_53c_goodB2GSink(char * data)
      |{
      |    CWE415_Double_Free__malloc_free_char_53d_goodB2GSink(data);
      |}
      |
      |void CWE415_Double_Free__malloc_free_char_53d_badSink(char * data)
      |{
      |    /* POTENTIAL FLAW: Possibly freeing memory twice */
      |    free(data);
      |}
      |
      |void CWE415_Double_Free__malloc_free_char_53d_goodG2BSink(char * data)
      |{
      |    /* POTENTIAL FLAW: Possibly freeing memory twice */
      |    free(data);
      |}
      |
      |/* goodB2G uses the BadSource with the GoodSink */
      |void CWE415_Double_Free__malloc_free_char_53d_goodB2GSink(char * data)
      |{
      |    /* do nothing */
      |    /* FIX: Don't attempt to free the memory */
      |    ; /* empty statement needed for some flow variants */
      |}
      |
      |""".stripMargin)

    "find flow for maxCallDepth = -1" in {
      def freeArg = cpg.call("free").argument(1)
      freeArg.reachableByFlows(freeArg).count(path => path.elements.size > 1) shouldBe 1
    }
  }

  "DataFlowTests68" should {
    val cpg = code("""
      |void CWE415_Double_Free__malloc_free_char_54b_badSink(char * data);
      |
      |void CWE415_Double_Free__malloc_free_char_54_bad()
      |{
      |    char * data;
      |    /* Initialize data */
      |    data = NULL;
      |    data = (char *)malloc(100*sizeof(char));
      |    if (data == NULL) {exit(-1);}
      |    /* POTENTIAL FLAW: Free data in the source - the bad sink frees data as well */
      |    free(data);
      |    CWE415_Double_Free__malloc_free_char_54b_badSink(data);
      |}
      |
      |void CWE415_Double_Free__malloc_free_char_54b_goodG2BSink(char * data);
      |
      |static void goodG2B()
      |{
      |    char * data;
      |    /* Initialize data */
      |    data = NULL;
      |    data = (char *)malloc(100*sizeof(char));
      |    if (data == NULL) {exit(-1);}
      |    /* FIX: Do NOT free data in the source - the bad sink frees data */
      |    CWE415_Double_Free__malloc_free_char_54b_goodG2BSink(data);
      |}
      |
      |/* goodB2G uses the BadSource with the GoodSink */
      |void CWE415_Double_Free__malloc_free_char_54b_goodB2GSink(char * data);
      |
      |static void goodB2G()
      |{
      |    char * data;
      |    /* Initialize data */
      |    data = NULL;
      |    data = (char *)malloc(100*sizeof(char));
      |    if (data == NULL) {exit(-1);}
      |    /* POTENTIAL FLAW: Free data in the source - the bad sink frees data as well */
      |    free(data);
      |    CWE415_Double_Free__malloc_free_char_54b_goodB2GSink(data);
      |}
      |
      |void CWE415_Double_Free__malloc_free_char_54_good()
      |{
      |    goodG2B();
      |    goodB2G();
      |}
      |
      |void CWE415_Double_Free__malloc_free_char_54c_badSink(char * data);
      |
      |void CWE415_Double_Free__malloc_free_char_54b_badSink(char * data)
      |{
      |    CWE415_Double_Free__malloc_free_char_54c_badSink(data);
      |}
      |
      |void CWE415_Double_Free__malloc_free_char_54c_goodG2BSink(char * data);
      |
      |void CWE415_Double_Free__malloc_free_char_54b_goodG2BSink(char * data)
      |{
      |    CWE415_Double_Free__malloc_free_char_54c_goodG2BSink(data);
      |}
      |
      |void CWE415_Double_Free__malloc_free_char_54c_goodB2GSink(char * data);
      |
      |void CWE415_Double_Free__malloc_free_char_54b_goodB2GSink(char * data)
      |{
      |    CWE415_Double_Free__malloc_free_char_54c_goodB2GSink(data);
      |}
      |
      |void CWE415_Double_Free__malloc_free_char_54d_badSink(char * data);
      |
      |void CWE415_Double_Free__malloc_free_char_54c_badSink(char * data)
      |{
      |    CWE415_Double_Free__malloc_free_char_54d_badSink(data);
      |}
      |
      |void CWE415_Double_Free__malloc_free_char_54d_goodG2BSink(char * data);
      |
      |void CWE415_Double_Free__malloc_free_char_54c_goodG2BSink(char * data)
      |{
      |    CWE415_Double_Free__malloc_free_char_54d_goodG2BSink(data);
      |}
      |
      |void CWE415_Double_Free__malloc_free_char_54d_goodB2GSink(char * data);
      |
      |void CWE415_Double_Free__malloc_free_char_54c_goodB2GSink(char * data)
      |{
      |    CWE415_Double_Free__malloc_free_char_54d_goodB2GSink(data);
      |}
      |
      |void CWE415_Double_Free__malloc_free_char_54e_badSink(char * data);
      |
      |void CWE415_Double_Free__malloc_free_char_54d_badSink(char * data)
      |{
      |    CWE415_Double_Free__malloc_free_char_54e_badSink(data);
      |}
      |
      |void CWE415_Double_Free__malloc_free_char_54e_goodG2BSink(char * data);
      |
      |void CWE415_Double_Free__malloc_free_char_54d_goodG2BSink(char * data)
      |{
      |    CWE415_Double_Free__malloc_free_char_54e_goodG2BSink(data);
      |}
      |
      |void CWE415_Double_Free__malloc_free_char_54e_goodB2GSink(char * data);
      |
      |void CWE415_Double_Free__malloc_free_char_54d_goodB2GSink(char * data)
      |{
      |    CWE415_Double_Free__malloc_free_char_54e_goodB2GSink(data);
      |}
      |
      |void CWE415_Double_Free__malloc_free_char_54e_badSink(char * data)
      |{
      |    /* POTENTIAL FLAW: Possibly freeing memory twice */
      |    free(data);
      |}
      |
      |void CWE415_Double_Free__malloc_free_char_54e_goodG2BSink(char * data)
      |{
      |    /* POTENTIAL FLAW: Possibly freeing memory twice */
      |    free(data);
      |}
      |
      |void CWE415_Double_Free__malloc_free_char_54e_goodB2GSink(char * data)
      |{
      |    /* do nothing */
      |    /* FIX: Don't attempt to free the memory */
      |    ; /* empty statement needed for some flow variants */
      |}
      |
      |""".stripMargin)

    "find flow for maxCallDepth = -1" in {
      def freeArg = cpg.call("free").argument(1)
      freeArg.reachableByFlows(freeArg).count(path => path.elements.size > 1) shouldBe 1
    }

  }

}
