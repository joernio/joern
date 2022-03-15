package io.joern.jssrc2cpg.dataflow

import io.joern.jssrc2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.joern.dataflowengineoss.language._
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.semanticcpg.language._
import org.scalatest.Suites
import overflowdb.traversal._

class DataFlowTestSuite
    extends Suites(
      new JSDataFlowTest1,
      new JSDataFlowTest2,
      new JSDataFlowTest3,
      new JSDataFlowTest4,
      new JSDataFlowTest5,
      new JSDataFlowTest6,
      new JSDataFlowTest7,
      new JSDataFlowTest8,
      new JSDataFlowTest9,
      new JSDataFlowTest10,
      new JSDataFlowTest11,
      new JSDataFlowTest12,
      new JSDataFlowTest13,
      new JSDataFlowTest14,
      new JSDataFlowTest15,
      new JSDataFlowTest16,
      new JSDataFlowTest17,
      new JSDataFlowTest18,
      new JSDataFlowTest19,
      new JSDataFlowTest20,
      new JSDataFlowTest21,
      new JSDataFlowTest22
    )

class JSDataFlowTest1 extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      | function flows1(fd, mode) {
      |     var buff = [];
      |
      |     var sz = 0;
      |     if (mode == 1) sz = 20;
      |     if (mode == 2) sz = 200;
      |     if (mode == 3) sz = 41;
      |     if (mode == 5) sz = -5;
      |
      |     read(fd, buff, sz);
      | };
      """.stripMargin

  "Flow from function call read to multiple versions of the same variable" in {
    def source = cpg.identifier.name("sz")
    def sink   = cpg.call.code("read.*")
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(
        List(("read(fd, buff, sz)", 11)),
        List(("sz = 0", 5), ("read(fd, buff, sz)", 11)),
        List(("sz = 20", 6), ("read(fd, buff, sz)", 11)),
        List(("sz = 200", 7), ("read(fd, buff, sz)", 11)),
        List(("sz = 41", 8), ("read(fd, buff, sz)", 11)),
        List(("sz = -5", 9), ("read(fd, buff, sz)", 11))
      )

    def flowsPretty = flows.p.mkString
    flowsPretty.should(include("sz = 20"))
    flowsPretty.should(include("read(fd, buff, sz)"))

    val tmpSourceFile = flows.head.elements.head.method.filename
    flowsPretty.should(include(tmpSourceFile))
  }
}

class JSDataFlowTest2 extends DataFlowCodeToCpgSuite {
  override val code: String =
    """
      | function foo(x) {};
      |
      | function method(y){
      |  var a = 10;
      |  if (a < y){
      |    foo(a);
      |  };
      | };
      """.stripMargin

  "Flow from function call argument" in {
    def source = cpg.identifier.name("a")
    def sink   = cpg.call.code("foo.*").argument
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(List(("foo(a)", 7)), List(("a = 10", 5), ("a < y", 6), ("foo(a)", 7)), List(("a < y", 6), ("foo(a)", 7)))
  }
}

class JSDataFlowTest3 extends DataFlowCodeToCpgSuite {
  override val code: String =
    """
      | function flow() {
      |   var a = 0x37;
      |   var b=a;
      |   var c=0x31;
      |   var z = b + c;
      |   z++;
      |   var p = z;
      |   var x = z;
      | };
      """.stripMargin

  "Flow chains from x to a" in {
    def source = cpg.identifier.name("a")
    def sink   = cpg.identifier.name("x")
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(
        List(("a = 55", 3), ("b = a", 4), ("b + c", 6), ("z = b + c", 6), ("z++", 7), ("x = z", 9)),
        List(("b = a", 4), ("b + c", 6), ("z = b + c", 6), ("z++", 7), ("x = z", 9))
      )
  }
}

class JSDataFlowTest4 extends DataFlowCodeToCpgSuite {
  override val code: String =
    """
      | function flow(a){
      |   var z = a;
      |   var b = z;
      |
      |   return b;
      | };
      """.stripMargin

  "Flow from method return to a" in {
    def source = cpg.identifier.name("a")
    def sink   = cpg.method(".*flow").ast.isReturn
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(List(("z = a", 3), ("b = z", 4), ("return b", 6)))
  }
}

class JSDataFlowTest5 extends DataFlowCodeToCpgSuite {
  override val code: String =
    """
      | function nested(a){
      |   var x = 0;
      |   var z = 1;
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

  "Flow with nested if-statements from method return to a" in {
    def source = cpg.call.code("a < 10").argument.code("a")
    def sink   = cpg.method(".*nested").ast.isReturn
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(List(("a < 10", 5), ("a < 5", 6), ("a < 2", 7), ("x = a", 8), ("return x", 14)))
  }
}

class JSDataFlowTest6 extends DataFlowCodeToCpgSuite {
  override val code: String =
    """
      | function nested(a) {
      |   var x = 0;
      |   var z = 1;
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
      | };
      """.stripMargin

  "Flow with nested if-statements to `return x`" in {
    def source = cpg.identifier.name("x")
    def sink   = cpg.method(".*nested").ast.isReturn
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(
        List(("return x", 14)),
        List(("x = z", 12), ("return x", 14)),
        List(("x = 0", 3), ("return x", 14)),
        List(("x = a", 8), ("return x", 14))
      )
  }
}

class JSDataFlowTest7 extends DataFlowCodeToCpgSuite {
  override val code: String =
    """
      | function foo(y) {};
      |
      | function param(x){
      |    var a = x;
      |    var b = a;
      |    var z = foo(b);
      |  }
      """.stripMargin

  "Flow chain from function argument of foo to a" in {
    def source = cpg.identifier.name("a")
    def sink   = cpg.call.code("foo.*").argument
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe Set(
      List(("a = x", 5), ("b = a", 6), ("foo(b)", 7)),
      List(("b = a", 6), ("foo(b)", 7))
    )

  }
}

class JSDataFlowTest8 extends DataFlowCodeToCpgSuite {
  override val code: String = """
      | function param(x){
      |    var a = x;
      |    var b = a;
      |    var z = foo(b);
      |  }
      """.stripMargin

  "Flow from function foo to a" in {
    def source = cpg.identifier.name("a")
    def sink   = cpg.call.code("foo.*").argument(1)
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(List(("a = x", 3), ("b = a", 4), ("foo(b)", 5)), List(("b = a", 4), ("foo(b)", 5)))
  }
}

class JSDataFlowTest9 extends DataFlowCodeToCpgSuite {
  override val code: String =
    """
      | var node = {
      |  'value1' : 1,
      |  'value2' : 2
      | };
      |
      |function test(){
      |  var x = 10;
      |  node.value1 = x;
      |  node.value2 = node.value1;
      |}
      """.stripMargin

  "Flow with member access in expression to identifier x" in {
    def source = cpg.identifier.name("x")
    def sink   = cpg.call.code("node.value2")
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(
        List(("node.value1 = x", 9), ("node.value2 = node.value1", 10)),
        List(("x = 10", 8), ("node.value1 = x", 9), ("node.value2 = node.value1", 10))
      )
  }
}

class JSDataFlowTest10 extends DataFlowCodeToCpgSuite {
  override val code: String =
    """
      | function flow() {
      |   var a = 37;
      |   var b = a;
      |   var c = 31;
      |   var z = b + c;
      |   z++;
      |   var p = z;
      |   var x = z;
      | }
      """.stripMargin

  "Flow chain from x to literal 37" in {
    def source = cpg.literal.code("37")
    def sink   = cpg.identifier.name("x")
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(List(("a = 37", 3), ("b = a", 4), ("b + c", 6), ("z = b + c", 6), ("z++", 7), ("x = z", 9)))
  }
}

class JSDataFlowTest11 extends DataFlowCodeToCpgSuite {
  override val code: String =
    """
      | function flow() {
      |    var a = 37;
      |    var b = a;
      |    var z = b;
      |    z+=a;
      | }
       """.stripMargin

  "Flow with short hand assignment operator" in {
    def source = cpg.call.code("a = 37").argument(2)
    def sink   = cpg.call.code("z \\+= a").argument(1)
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(List(("a = 37", 3), ("b = a", 4), ("z = b", 5), ("z += a", 6)))
  }
}

class JSDataFlowTest12 extends DataFlowCodeToCpgSuite {
  override val code: String =
    """
      | function flow() {
      |    var a = 37;
      |    var b = a;
      |    var z = b;
      |    z += a;
      |    var w = z;
      | }
      """.stripMargin

  "Flow after short hand assignment" in {
    def source = cpg.call.code("a = 37").argument(1)
    def sink   = cpg.identifier.name("w")
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(List(("a = 37", 3), ("b = a", 4), ("z = b", 5), ("z += a", 6), ("w = z", 7)))
  }
}

class JSDataFlowTest13 extends DataFlowCodeToCpgSuite {
  override val code: String =
    """
      | function main(argc, argv){
      |    var x = argv[1];
      |    var y = x;
      |    var z = y;
      |    return 0;
      | };
      """.stripMargin

  "Flow from array method parameter to identifier" in {
    def source = cpg.method(".*main").parameter
    def sink   = cpg.identifier.name("y")
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(
        List(("main(this, argc, argv)", 2), ("x = argv[1]", 3), ("y = x", 4), ("z = y", 5)),
        List(("main(this, argc, argv)", 2), ("x = argv[1]", 3), ("y = x", 4))
      )
  }
}

class JSDataFlowTest14 extends DataFlowCodeToCpgSuite {
  override val code: String =
    """
       |function foo(x, y) {
       |  var z =  x ? f(y) : g(y);
       |  return;
       | }
      """.stripMargin

  "Flow for conditional expressions" in {
    def source = cpg.method.parameter.name("y")
    def sink   = cpg.identifier.name("z")
    def flows  = sink.reachableByFlows(source)
    flows.size shouldBe 1
  }
}

class JSDataFlowTest15 extends DataFlowCodeToCpgSuite {
  override val code: String = """
    |function bar() {
    |  var x = source();
    |  foo(x);
    |};
    |
    |function foo(y) {
    |  sink(y);
    |};""".stripMargin

  "Flow for source in caller" in {
    def source = cpg.call.code("source.*")
    def sink   = cpg.call.code("sink.*").argument(1)
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe Set(
      List(("source()", 3), ("x = source()", 3), ("foo(x)", 4), ("foo(this, y)", 7), ("sink(y)", 8))
    )
  }
}

class JSDataFlowTest16 extends DataFlowCodeToCpgSuite {
  override val code: String = """
    |function bar() {
    |  return source();
    |};
    |
    |function sink(param) {};
    |
    |function foo(y) {
    |  var y = bar();
    |  sink(y);
    |};""".stripMargin

  "Flow for source in callee" in {
    def source = cpg.call.code("source.*")
    def sink   = cpg.call.code("sink.*").argument(1)
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe Set(
      List(("source()", 3), ("return source()", 3), ("RET", 2), ("bar()", 9), ("y = bar()", 9), ("sink(y)", 10))
    )
  }

  "Flow for using formal parameters as sink" in {
    def source = cpg.call.code("source.*")
    def sink   = cpg.method(".*sink").parameter.index(1)
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe Set(
      List(
        ("source()", 3),
        ("return source()", 3),
        ("RET", 2),
        ("bar()", 9),
        ("y = bar()", 9),
        ("sink(y)", 10),
        ("sink(this, param)", 6)
      )
    )
  }
}

class JSDataFlowTest17 extends DataFlowCodeToCpgSuite {
  override val code: String =
    """
      | var point = {
      |   'x' : 0,
      |   'y' : 0
      | };
      |
      | function source() {
      |   return 2.0;
      | };
      |
      | function sink(x) {
      |   return 3;
      | };
      |
      | function main() {
      |   var k = source();
      |   point.x = k;
      |   point.y = 2;
      |   sink(point.x);
      | };
      |""".stripMargin

  "Flow for struct data" in {
    def source = cpg.call.code("source.*")
    def sink   = cpg.call.code("sink.*").argument
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe Set(
      List(("source()", 16), ("k = source()", 16), ("point.x = k", 17), ("sink(point.x)", 19))
    )

  }
}

class JSDataFlowTest18 extends DataFlowCodeToCpgSuite {
  override val code: String =
    """
      | var s { 'field' : 0 };
      |
      | function foo(arg) {
      |   arg.field = source();
      |   sink(arg.field);
      | }
      |""".stripMargin

  "Flow for object element access" in {
    def source = cpg.call.code("source.*")
    def sink   = cpg.call.code("sink.*").argument
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(List(("source()", 5), ("arg.field = source()", 5), ("sink(arg.field)", 6)))
  }

}

class JSDataFlowTest19 extends DataFlowCodeToCpgSuite {
  override val code: String =
    """
      |function bar() {
      |  source(a.b);
      |  sink(a.b);
      |}
      |
      |""".stripMargin

  "Flow for object element access passed to source" in {
    def source = cpg.call.code("source.*").argument
    def sink   = cpg.call.code("sink.*").argument
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe Set(List(("source(a.b)", 3), ("sink(a.b)", 4)))
  }
}

class JSDataFlowTest20 extends DataFlowCodeToCpgSuite {
  override val code: String =
    """
      |function foo(y, x) {
      |  free(y);
      |  free(x);
      |};
      |""".stripMargin

  "Flows for statements to METHOD_RETURN" in {
    def source = cpg.call.code("free.*").argument(1)
    def sink   = cpg.method(".*foo").methodReturn
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(List(("free(y)", 3), ("RET", 2)), List(("free(x)", 4), ("RET", 2)))
  }
}

class JSDataFlowTest21 extends DataFlowCodeToCpgSuite {
  override val code: String =
    """
      | function foo() {
      |   return bar();
      | };
    """.stripMargin

  "Should not create edges from call to ret twice" in {
    cpg.call
      .code("bar.*")
      .outE(EdgeTypes.REACHING_DEF)
      .count(_.inNode() == cpg.ret.head) shouldBe 1
  }
}

class JSDataFlowTest22 extends DataFlowCodeToCpgSuite {
  override val code: String =
    """
       | function f(x, y) {
       |   g(x, y);
       | };""".stripMargin

  "Flow from outer params to inner params" in {
    def source = cpg.method(".*f").parameter
    def sink   = cpg.call.code("g.*").argument
    sink.size shouldBe 3   // incl. this
    source.size shouldBe 3 // incl. this

    def flows = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(List(("f(this, x, y)", 2), ("g(x, y)", 3)))
  }
}
