package io.shiftleft.c2cpg.querying

import io.shiftleft.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.dataflowengineoss.language._
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.Traversal

class CDataFlowTests1 extends DataFlowCodeToCpgSuite {

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

  "Test 1: flow from function call read to multiple versions of the same variable" in {

    val source = cpg.identifier.name("sz").l
    val sink = cpg.call.name("read").l
    def flows = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSet shouldBe
      Set(
        List[(String, Option[Integer])](
          ("sz = 200", 8),
          ("read(fd, buff, sz)", 12)
        ),
        List[(String, Option[Integer])](
          ("sz = -5", 10),
          ("read(fd, buff, sz)", 12)
        ),
        List[(String, Option[Integer])](
          ("sz = 41", 9),
          ("read(fd, buff, sz)", 12)
        ),
        List[(String, Option[Integer])](
          ("sz = 0", 6),
          ("read(fd, buff, sz)", 12)
        ),
        List[(String, Option[Integer])](
          ("sz = 20", 7),
          ("read(fd, buff, sz)", 12)
        ),
        List[(String, Option[Integer])](
          ("read(fd, buff, sz)", 12)
        )
      )

    // pretty printing for flows
    val flowsPretty = flows.p.mkString
    flowsPretty.should(include("sz = 20"))
    flowsPretty.should(include("read(fd, buff, sz)"))
    val tmpSourceFile = flows.head.elements.head.method.filename
    flowsPretty.should(include(tmpSourceFile))
  }
}

class CDataFlowTests2 extends DataFlowCodeToCpgSuite {
  override val code =
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

  "Test 2: flow with pointers" in {
    implicit val callResolver = NoResolve
    val source = cpg.identifier
    val sink = cpg.method.name("free").parameter.argument
    val flows = sink
      .reachableByFlows(source)
      .l
      .map(flowToResultPairs)
      .distinct

    flows.size shouldBe 5

    flows.toSet shouldBe
      Set(
        List[(String, Option[Integer])](
          ("*p = head", 8),
          ("p != NULL", 8),
          ("free(p)", 10)
        ),
        List[(String, Option[Integer])](
          ("q = p->next", 9),
          ("p = q", 8),
          ("p != NULL", 8),
          ("free(p)", 10)
        ),
        List[(String, Option[Integer])](
          ("p = q", 8),
          ("p != NULL", 8),
          ("free(p)", 10)
        ),
        List[(String, Option[Integer])](
          ("p != NULL", 8),
          ("free(p)", 10)
        ),
        List[(String, Option[Integer])](
          ("free(p)", 10)
        )
      )
  }
}

class CDataFlowTests3 extends DataFlowCodeToCpgSuite {
  override val code =
    """
        | int method(int y){
        |  int a = 10;
        |  if (a < y){
        |    foo(a);
        |  }
        | }
      """.stripMargin

  "Test 3: flow from function call argument" in {
    implicit val callResolver = NoResolve
    val source = cpg.identifier.name("a")
    val sink = cpg.method.name("foo").parameter.argument
    val flows = sink.reachableByFlows(source).l

    flows.size shouldBe 3

    flows.map(flowToResultPairs).toSet shouldBe
      Set(
        List[(String, Option[Integer])](
          ("a = 10", 3),
          ("a < y", 4),
          ("foo(a)", 5)
        ),
        List[(String, Option[Integer])](
          ("a < y", 4),
          ("foo(a)", 5)
        ),
        List[(String, Option[Integer])](
          ("foo(a)", 5)
        ),
      )
  }
}

class CDataFlowTests4 extends DataFlowCodeToCpgSuite {
  override val code =
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

  "Test 4: flow chains from x to a" in {
    val source = cpg.identifier.name("a")
    val sink = cpg.identifier.name("x")
    val flows = sink.reachableByFlows(source).l

    flows.size shouldBe 2
    flows.map(flowToResultPairs).toSet shouldBe
      Set(
        List[(String, Option[Integer])](
          ("a = 0x37", 3),
          ("b=a", 4),
          ("b + c", 6),
          ("z = b + c", 6),
          ("z++", 7),
          ("x = z", 9)
        ),
        List[(String, Option[Integer])](
          ("b=a", 4),
          ("b + c", 6),
          ("z = b + c", 6),
          ("z++", 7),
          ("x = z", 9)
        )
      )
  }
}

class CDataFlowTests5 extends DataFlowCodeToCpgSuite {
  override val code =
    """
        | int flow(int a){
        |   int z = a;
        |   int b = z;
        |
        |   return b;
        | }
      """.stripMargin

  "Test 5: flow from method return to a" in {
    val source = cpg.identifier.name("a")
    val sink = cpg.method("flow").ast.isReturn
    val flows = sink.reachableByFlows(source).l

    flows.size shouldBe 1

    flows.map(flowToResultPairs).toSet shouldBe
      Set(
        List[(String, Option[Integer])](
          ("z = a", 3),
          ("b = z", 4),
          ("return b;", 6),
        ))
  }
}

class CDataFlowTests6 extends DataFlowCodeToCpgSuite {
  override val code =
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

  "Test 6: flow with nested if-statements from method return to a" in {
    val source = cpg.call.code("a < 10").argument.code("a")
    val sink = cpg.method("nested").ast.isReturn
    val flows = sink.reachableByFlows(source).l

    flows.size shouldBe 1

    flows.map(flowToResultPairs).toSet shouldBe
      Set(
        List[(String, Option[Integer])](
          ("a < 10", Some(5)),
          ("a < 5", Some(6)),
          ("a < 2", Some(7)),
          ("x = a", 8),
          ("return x;", 14),
        ))
  }
}

class CDataFlowTests7 extends DataFlowCodeToCpgSuite {
  override val code =
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

  "Test 7: flow with nested if-statements to `return x`" in {
    val source = cpg.identifier.name("x")
    val sink = cpg.method("nested").ast.isReturn
    val flows = sink.reachableByFlows(source).l

    flows.size shouldBe 3

    flows.map(flowToResultPairs).toSet shouldBe
      Set(
        List[(String, Option[Integer])](
          ("x = z", 12),
          ("return x;", 14),
        ),
        List[(String, Option[Integer])](
          ("x = a", 8),
          ("return x;", 14),
        ),
        List[(String, Option[Integer])](
          ("return x;", 14),
        )
      )
  }
}

class CDataFlowTests8 extends DataFlowCodeToCpgSuite {
  override val code =
    """
        | void param(int x){
        |    int a = x;
        |    int b = a;
        |    int z = foo(b);
        |  }
      """.stripMargin

  "Test 8: flow chain from function argument of foo to a" in {
    implicit val callResolver = NoResolve
    val source = cpg.identifier.name("a")
    val sink = cpg.method.name("foo").parameter.argument
    val flows = sink.reachableByFlows(source).l

    flows.size shouldBe 2

    flows.map(flowToResultPairs).toSet shouldBe
      Set(List[(String, Option[Integer])](
            ("a = x", 3),
            ("b = a", 4),
            ("foo(b)", 5)
          ),
          List[(String, Option[Integer])](
            ("b = a", 4),
            ("foo(b)", 5)
          ))

  }
}

class CDataFlowTests9 extends DataFlowCodeToCpgSuite {
  override val code = """
                          | void param(int x){
                          |    int a = x;
                          |    int b = a;
                          |    int z = foo(b);
                          |  }
      """.stripMargin

  "Test 9: flow from function foo to a" in {
    val source = cpg.identifier.name("a")
    val sink = cpg.call.name("foo").argument(1)
    val flows = sink.reachableByFlows(source).l

    flows.size shouldBe 2

    flows.map(flowToResultPairs).toSet shouldBe
      Set(List[(String, Option[Integer])](
            ("a = x", 3),
            ("b = a", 4),
            ("foo(b)", 5)
          ),
          List[(String, Option[Integer])](
            ("b = a", 4),
            ("foo(b)", 5)
          ))
  }
}

class CDataFlowTests10 extends DataFlowCodeToCpgSuite {
  override val code =
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

  "Test 10: flow with member access in expression to identifier x" in {
    val source = cpg.identifier.name("x")
    val sink = cpg.call.code("n.value2")
    val flows = sink.reachableByFlows(source).l

    flows.size shouldBe 2

    flows.map(flowToResultPairs).toSet shouldBe
      Set(
        List[(String, Option[Integer])](
          ("x = 10", 8),
          ("n.value1 = x", 10),
          ("n.value2 = n.value1", 11)
        ),
        List[(String, Option[Integer])](
          ("n.value1 = x", 10),
          ("n.value2 = n.value1", 11)
        )
      )
  }
}

class CDataFlowTests11 extends DataFlowCodeToCpgSuite {
  override val code =
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

  "Test 11: flow chain from x to literal 0x37" in {
    val source = cpg.literal.code("0x37")
    val sink = cpg.identifier.name("x")
    val flows = sink.reachableByFlows(source).l

    flows.size shouldBe 1

    flows.map(flowToResultPairs).toSet shouldBe
      Set(
        List[(String, Option[Integer])](
          ("a = 0x37", 3),
          ("b=a", 4),
          ("b + c", 6),
          ("z = b + c", 6),
          ("z++", Some(7)),
          ("x = z", 9)
        ))
  }
}

class CDataFlowTests12 extends DataFlowCodeToCpgSuite {
  override val code =
    """
      | void flow(void) {
      |    int a = 0x37;
      |    int b = a;
      |    int z = b;
      |    z+=a;
      | }
       """.stripMargin

  "Test 12: flow with short hand assignment operator" in {
    val source = cpg.call.code("a = 0x37").argument(2)
    val sink = cpg.call.code("z\\+=a").argument(1)
    val flows = sink.reachableByFlows(source).l

    flows.size shouldBe 1

    flows.map(flowToResultPairs).toSet shouldBe
      Set(
        List[(String, Option[Integer])](
          ("a = 0x37", 3),
          ("b = a", 4),
          ("z = b", 5),
          ("z+=a", 6)
        ))
  }
}

class CDataFlowTests13 extends DataFlowCodeToCpgSuite {
  override val code =
    """
        | void flow(void) {
        |    int a = 0x37;
        |    int b = a;
        |    int z = b;
        |    z+=a;
        |    int w = z;
        | }
      """.stripMargin

  "Test 13: flow after short hand assignment" in {
    val source = cpg.call.code("a = 0x37").argument(1)
    val sink = cpg.identifier.name("w")
    val flows = sink.reachableByFlows(source).l

    flows.size shouldBe 1

    flows.map(flowToResultPairs).toSet shouldBe
      Set(
        List[(String, Option[Integer])](
          ("a = 0x37", 3),
          ("b = a", 4),
          ("z = b", 5),
          ("z+=a", 6),
          ("w = z", 7)
        )
      )
  }
}

class CDataFlowTests14 extends DataFlowCodeToCpgSuite {
  override val code =
    """
        | int main(int argc, char** argv){
        |    int x = argv[1];
        |    int y = x;
        |    int z = y;
        |
        |    return 0;
        | }
      """.stripMargin

  "Test 14: flow from array method parameter to identifier" in {
    val source = cpg.method.parameter
    val sink = cpg.identifier.name("y")
    val flows = sink.reachableByFlows(source).l
    flows.size shouldBe 2

    flows.map(flowToResultPairs).toSet shouldBe
      Set(
        List[(String, Option[Integer])](
          ("main(int argc, char** argv)", 2),
          ("x = argv[1]", 3),
          ("y = x", 4),
          ("z = y", 5)
        ),
        List[(String, Option[Integer])](
          ("main(int argc, char** argv)", 2),
          ("x = argv[1]", 3),
          ("y = x", 4)
        )
      )
  }
}

class CDataFlowTests15 extends DataFlowCodeToCpgSuite {
  override val code =
    """
  void foo(bool x, void* y) {
    void* z =  x ? f(y) : g(y);
    return;
  }
      """.stripMargin

  "Test 15: conditional expressions (joern issue #91)" in {
    val source = cpg.method.parameter.name("y")
    val sink = cpg.identifier.name("z")
    val flows = sink.reachableByFlows(source).l
    flows.size shouldBe 1
  }
}

class CDataFlowTests16 extends DataFlowCodeToCpgSuite {
  override val code = """

  int bar() {
    int x = source();
    foo(x);
  }

  void foo(int y) {
    sink(y);
  }

  """.stripMargin

  "Test 16: find source in caller" in {
    val source = cpg.call("source")
    val sink = cpg.call("sink").argument(1)
    sink.reachableByFlows(source).l.map(flowToResultPairs).toSet shouldBe Set(
      List(("source()", Some(4)),
           ("x = source()", Some(4)),
           ("foo(x)", Some(5)),
           ("foo(int y)", Some(8)),
           ("sink(y)", Some(9))))
  }
}

class CDataFlowTests17 extends DataFlowCodeToCpgSuite {
  override val code = """

  int bar() {
    return source();
  }

  void foo(int y) {
    int y = bar();
    sink(y);
  }

  """.stripMargin

  "Test 17.1: find source in callee" in {
    val source = cpg.call("source")
    val sink = cpg.call("sink").argument(1)
    sink.reachableByFlows(source).l.map(flowToResultPairs).toSet shouldBe Set(
      List(("source()", Some(4)),
           ("return source();", Some(4)),
           ("int", Some(3)),
           ("bar()", Some(8)),
           ("y = bar()", Some(8)),
           ("sink(y)", Some(9)))
    )
  }

  "Test 17.2 : allow using formal parameters as sink" in {
    val source = cpg.call("source")
    val sink = cpg.method("sink").parameter.index(1)
    sink.reachableByFlows(source).l.map(flowToResultPairs).toSet shouldBe Set(
      List(("source()", Some(4)),
           ("return source();", Some(4)),
           ("int", Some(3)),
           ("bar()", Some(8)),
           ("y = bar()", Some(8)),
           ("sink(y)", Some(9)),
           ("sink(p1)", None))
    )
  }
}

class CDataFlowTests18 extends DataFlowCodeToCpgSuite {
  override val code =
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

  "Test 18: struct data flow" in {
    val source = cpg.method.name("source").methodReturn
    val sink = cpg.method.name("sink").parameter.name("x")

    val flows = sink.reachableByFlows(source).l
    flows.size shouldBe 1
    flows.map(flowToResultPairs).toSet shouldBe Set(
      List(("double", Some(7)),
           ("source(2)", Some(16)),
           ("k = source(2)", Some(16)),
           ("point.x = k", Some(18)),
           ("sink(point.x)", Some(20)),
           ("sink(int x)", Some(11)))
    )

  }
}

class CDataFlowTests19 extends DataFlowCodeToCpgSuite {
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

  "Test 19: tainted struct" in {
    val source = cpg.method.name("source").methodReturn
    val sink = cpg.method.name("sink").parameter.name("x")
    val flows = sink.reachableByFlows(source).l

    flows.size shouldBe 1
    flows.map(flowToResultPairs).toSet shouldBe Set(
      List(("struct Point", Some(7)),
           ("source(2)", Some(17)),
           ("point = source(2)", Some(17)),
           ("sink(point.x)", Some(18)),
           ("sink(int x)", Some(12)))
    )
  }
}

class CDataFlowTests20 extends DataFlowCodeToCpgSuite {
  override val code =
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

  "Test 20: should not find any flows" in {
    val source = cpg.method.name("source").methodReturn
    val sink = cpg.method.name("sink").parameter
    val flows = sink.reachableByFlows(source).l
    flows.size shouldBe 0
  }
}

class CDataFlowTests21 extends DataFlowCodeToCpgSuite {
  override val code =
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

  "Test 21: should find flow" in {
    val source = cpg.method.name("source").methodReturn
    val sink = cpg.method.name("sink").parameter
    val flows = sink.reachableByFlows(source).l
    flows.size shouldBe 1
  }
}

class CDataFlowTests22 extends DataFlowCodeToCpgSuite {
  override val code =
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

  "Test 22: find flows (pointer-to-struct/arrows vs star-dot)" in {
    val source = cpg.method.name("source").methodReturn
    val sink = cpg.method.name("sink").parameter
    val flows = sink.reachableByFlows(source).l
    flows.size shouldBe 1
  }

}

class CDataFlowTests23 extends DataFlowCodeToCpgSuite {
  override val code = """
      | int source();
      | void sink(int i);
      |
      | void foo(int* arg) {
      |   arg[0] = source();
      |   sink(*arg);
      | }
      |""".stripMargin

  "Test 23: handle deref vs array access correctly" in {
    val source = cpg.method.name("source").methodReturn
    val sink = cpg.call.codeExact("*arg")
    val flows = sink.reachableByFlows(source).l
    flows.size shouldBe 1
  }

}

class CDataFlowTests24 extends DataFlowCodeToCpgSuite {
  override val code =
    """
      |int foo() {
      |  source(&a->c);
      |  sink(a->b);
      |}
      |""".stripMargin

  "Test 24: should not report flow if access path differs" in {
    val source = cpg.call.name("source").argument.l
    val sink = cpg.method.name("sink").parameter.l
    val flows = sink.to(Traversal).reachableByFlows(source.to(Traversal)).l
    flows.size shouldBe 0
  }
}

class CDataFlowTests25 extends DataFlowCodeToCpgSuite {
  override val code =
    """
      |int bar() {
      |  source(&a->b);
      |  sink(a->b);
      |}
      |
      |""".stripMargin

  "Test 25: should report flow if address passed to source" in {
    val source = cpg.call("source").argument.l
    val sink = cpg.method.name("sink").parameter.l

    val flows = sink.reachableByFlows(source).l
    flows.map(flowToResultPairs).toSet shouldBe Set(
      List(("source(&a->b)", Some(3)), ("sink(a->b)", Some(4)), ("sink(p1)", None))
    )
  }
}

class CDataFlowTests26 extends DataFlowCodeToCpgSuite {
  override val code =
    """
      |int foo() {
      |  a->b = source();
      |  a->b = 10;
      |  sink(a->b);
      |}
      |
      |""".stripMargin

  "Test 26: should not report flow" in {
    val source = cpg.call.name("source").l
    val sink = cpg.method.name("sink").parameter.l
    val flows = sink.to(Traversal).reachableByFlows(source.to(Traversal)).l
    flows.size shouldBe 0

    val source2 = cpg.assignment.codeExact("a->b = 10").target.l
    val sink2 = cpg.method.name("sink").parameter.l
    source2.size shouldBe 1
    sink2.size shouldBe 1
    sink2.reachableBy(source2).size shouldBe 1
  }
}

class CDataFlowTests27 extends DataFlowCodeToCpgSuite {
  override val code =
    """
      |int foo(int y, int x) {
      |  free(y);
      |  free(x);
      |}
      |
      |""".stripMargin

  "Test 27: find flows of last statements to METHOD_RETURN" in {
    val source = cpg.call("free").argument(1)
    val sink = cpg.method("foo").methodReturn
    val flows = sink.reachableByFlows(source).l

    flows.map(flowToResultPairs).toSet shouldBe Set(
      List(("free(x)", Some(4)), ("int", Some(2))),
      List(("free(y)", Some(3)), ("int", Some(2)))
    )
  }
}

class CDataFlowTests28 extends DataFlowCodeToCpgSuite {
  override val code =
    """
      |int foo() {
      |   int y = 1;
      |   y = something_else;
      |   y = 10;
      |}
      |
      |""".stripMargin

  "Test 28: find that there is no flow from `y = 1` to exit node" in {
    val source = cpg.literal("1").l
    val sink = cpg.method("foo").methodReturn

    val flows = sink.reachableByFlows(source).l
    flows.size shouldBe 0
  }
}

class CDataFlowTests29 extends DataFlowCodeToCpgSuite {
  override val code =
    """
      |int foo() {
      |   char * y = malloc(10);
      |   free(y);
      |   y = 10;
      |}
      |
      |""".stripMargin

  "Test 29: find that there is no flow from free(y) to exit node" in {
    val source = cpg.call("free").argument(1).l
    val sink = cpg.method("foo").methodReturn.l

    val flows = sink.reachableByFlows(source).l
    flows.size shouldBe 0
  }
}

class CDataFlowTests30 extends DataFlowCodeToCpgSuite {
  override val code =
    """
      |int foo(int b) {
      |  b = source();
      |  b = 10;
      |  sink(b);
      |}
      |
      |""".stripMargin

  "Test 30: should block flow even if variable decl cannot be found" in {
    val source = cpg.call.name("source").l
    val sink = cpg.method.name("sink").parameter.l
    val flows = sink.to(Traversal).reachableByFlows(source.to(Traversal)).l
    flows.size shouldBe 0

    val source2 = cpg.assignment.codeExact("b = 10").target.l
    val sink2 = cpg.method.name("sink").parameter.l
    source2.size shouldBe 1
    sink2.size shouldBe 1
    sink2.reachableBy(source2).size shouldBe 1
  }
}

class CDataFlowTests31 extends DataFlowCodeToCpgSuite {
  override val code =
    """
      int foo() {
         return bar();
      }
    """

  "Test 31: should not create edges from call to ret twice" in {
    cpg
      .call("bar")
      .outE(EdgeTypes.REACHING_DEF)
      .count(_.inNode() == cpg.ret.head) shouldBe 1
  }
}

class CDataFlowTests32 extends DataFlowCodeToCpgSuite {
  override val code =
    """
       void f(int x, int y)
        {
          g(x, y);
        }
    """

  "Test 32: should find flow from outer params to inner params" in {
    def source = cpg.method.name("f").parameter
    def sink = cpg.method.name("g").parameter
    sink.size shouldBe 2
    source.size shouldBe 2
    sink.reachableBy(source).size shouldBe 4
  }
}
