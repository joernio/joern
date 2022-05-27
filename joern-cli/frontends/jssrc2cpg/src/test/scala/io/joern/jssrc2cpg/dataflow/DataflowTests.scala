package io.joern.jssrc2cpg.dataflow

import io.joern.jssrc2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.joern.dataflowengineoss.language._
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal._

class DataflowTests extends DataFlowCodeToCpgSuite {

  "Flow from function call read to multiple versions of the same variable" in {
    val cpg: Cpg = code("""
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
        | }
      """.stripMargin)

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

  "Flow from function call argument" in {
    val cpg: Cpg = code("""
        | function foo(x) {}
        |
        | function method(y){
        |  var a = 10;
        |  if (a < y){
        |    foo(a);
        |  };
        | }
      """.stripMargin)

    def source = cpg.identifier.name("a")
    def sink   = cpg.call.code("foo.*").argument
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(List(("foo(a)", 7)), List(("a = 10", 5), ("a < y", 6), ("foo(a)", 7)), List(("a < y", 6), ("foo(a)", 7)))
  }

  "Flow chains from x to a" in {
    val cpg: Cpg = code("""
        | function flow() {
        |   var a = 0x37;
        |   var b = a;
        |   var c = 0x31;
        |   var z = b + c;
        |   z++;
        |   var p = z;
        |   var x = z;
        | }
      """.stripMargin)

    def source = cpg.identifier.name("a")
    def sink   = cpg.identifier.name("x")
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(
        List(("a = 0x37", 3), ("b = a", 4), ("b + c", 6), ("z = b + c", 6), ("z++", 7), ("x = z", 9)),
        List(("b = a", 4), ("b + c", 6), ("z = b + c", 6), ("z++", 7), ("x = z", 9))
      )
  }

  "Flow from method return to a" in {
    val cpg: Cpg = code("""
        | function flow(a){
        |   var z = a;
        |   var b = z;
        |
        |   return b;
        | };
      """.stripMargin)

    def source = cpg.identifier.name("a")
    def sink   = cpg.method(".*flow").ast.isReturn
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(List(("z = a", 3), ("b = z", 4), ("return b", 6)))
  }

  "Flow with nested if-statements from method return to a" in {
    val cpg: Cpg = code("""
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
      """.stripMargin)

    def source = cpg.call.code("a < 10").argument.code("a")
    def sink   = cpg.method(".*nested").ast.isReturn
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(List(("a < 10", 5), ("a < 5", 6), ("a < 2", 7), ("x = a", 8), ("return x", 14)))
  }

  "Flow with nested if-statements to `return x`" in {
    val cpg: Cpg = code("""
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
      """.stripMargin)

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

  "Flow chain from function argument of foo to a" in {
    val cpg: Cpg = code("""
        | function foo(y) {};
        |
        | function param(x){
        |    var a = x;
        |    var b = a;
        |    var z = foo(b);
        |  }
      """.stripMargin)

    def source = cpg.identifier.name("a")
    def sink   = cpg.call.code("foo.*").argument
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe Set(
      List(("b = a", 6), ("foo(b)", 7)),
      List(("a = x", 5), ("b = a", 6), ("foo(b)", 7)),
      List(("a = x", 5), ("b = a", 6), ("foo(b)", 7), ("foo(this, y)", 2), ("RET", 2), ("foo(b)", 7)),
      List(("b = a", 6), ("foo(b)", 7), ("foo(this, y)", 2), ("RET", 2), ("foo(b)", 7))
    )

  }

  "Flow from function foo to a" in {
    val cpg: Cpg = code("""
        | function param(x){
        |    var a = x;
        |    var b = a;
        |    var z = foo(b);
        |  }
      """.stripMargin)

    def source = cpg.identifier.name("a")
    def sink   = cpg.call.code("foo.*").argument(1)
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(List(("a = x", 3), ("b = a", 4), ("foo(b)", 5)), List(("b = a", 4), ("foo(b)", 5)))
  }

  "Flow with member access in expression to identifier x" in {
    val cpg: Cpg = code("""
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
      """.stripMargin)

    def source = cpg.identifier.name("x")
    def sink   = cpg.call.code("node.value2")
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(
        List(("node.value1 = x", 9), ("node.value2 = node.value1", 10)),
        List(("x = 10", 8), ("node.value1 = x", 9), ("node.value2 = node.value1", 10))
      )
  }

  "Flow chain from x to literal 37" in {
    val cpg: Cpg = code("""
        | function flow() {
        |   var a = 37;
        |   var b = a;
        |   var c = 31;
        |   var z = b + c;
        |   z++;
        |   var p = z;
        |   var x = z;
        | }
      """.stripMargin)

    def source = cpg.literal.code("37")
    def sink   = cpg.identifier.name("x")
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(List(("a = 37", 3), ("b = a", 4), ("b + c", 6), ("z = b + c", 6), ("z++", 7), ("x = z", 9)))
  }

  "Flow with short hand assignment operator" in {
    val cpg: Cpg = code("""
        | function flow() {
        |    var a = 37;
        |    var b = a;
        |    var z = b;
        |    z += a;
        | }
       """.stripMargin)

    def source = cpg.call.code("a = 37").argument(2)
    def sink   = cpg.call.code("z \\+= a").argument(1)
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(List(("a = 37", 3), ("b = a", 4), ("z = b", 5), ("z += a", 6)))
  }

  "Flow after short hand assignment" in {
    val cpg: Cpg = code("""
        | function flow() {
        |    var a = 37;
        |    var b = a;
        |    var z = b;
        |    z += a;
        |    var w = z;
        | }
      """.stripMargin)

    def source = cpg.call.code("a = 37").argument(1)
    def sink   = cpg.identifier.name("w")
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(List(("a = 37", 3), ("b = a", 4), ("z = b", 5), ("z += a", 6), ("w = z", 7)))
  }

  "Flow from array method parameter to identifier" in {
    val cpg: Cpg = code("""
        | function main(argc, argv){
        |    var x = argv[1];
        |    var y = x;
        |    var z = y;
        |    return 0;
        | }
      """.stripMargin)

    def source = cpg.method(".*main").parameter
    def sink   = cpg.identifier.name("y")
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(
        List(("main(this, argc, argv)", 2), ("x = argv[1]", 3), ("y = x", 4), ("z = y", 5)),
        List(("main(this, argc, argv)", 2), ("x = argv[1]", 3), ("y = x", 4))
      )
  }

  "Flow for conditional expressions" in {
    val cpg: Cpg = code("""
        |function foo(x, y) {
        |  var z =  x ? f(y) : g(y);
        |  return;
        | }
      """.stripMargin)

    def source = cpg.method.parameter.name("y")
    def sink   = cpg.identifier.name("z")
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe Set(
      List(("foo(this, x, y)", 2), ("f(y)", 3), ("x ? f(y) : g(y)", 3), ("z =  x ? f(y) : g(y)", 3))
    )
  }

  "Flow for source in caller" in {
    val cpg: Cpg = code("""
        |function bar() {
        |  var x = source();
        |  foo(x);
        |}
        |
        |function foo(y) {
        |  sink(y);
        |}""".stripMargin)

    def source = cpg.call.code("source.*")
    def sink   = cpg.call.code("sink.*").argument
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe Set(
      List(("source()", 3), ("x = source()", 3), ("foo(x)", 4), ("foo(this, y)", 7), ("sink(y)", 8))
    )
  }

  "Flow for source in callee" in {
    val cpg: Cpg = code("""
        |function bar() {
        |  return source();
        |}
        |
        |function sink(param) {}
        |
        |function foo(y) {
        |  var y = bar();
        |  sink(y);
        |}""".stripMargin)

    def source = cpg.call.code("source.*")
    def sink   = cpg.call.code("sink.*").argument(1)
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe Set(
      List(("source()", 3), ("return source()", 3), ("RET", 2), ("bar()", 9), ("y = bar()", 9), ("sink(y)", 10))
    )
  }

  "Flow for using formal parameters as sink" in {
    val cpg: Cpg = code("""
        |function bar() {
        |  return source();
        |}
        |
        |function sink(param) {}
        |
        |function foo(y) {
        |  var y = bar();
        |  sink(y);
        |}""".stripMargin)

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

  "Flow for struct data" in {
    val cpg: Cpg = code("""
        | var point = {
        |   'x' : 0,
        |   'y' : 0
        | }
        |
        | function source() {
        |   return 2.0;
        | }
        |
        | function sink(x) {
        |   return 3;
        | }
        |
        | function main() {
        |   var k = source();
        |   point.x = k;
        |   point.y = 2;
        |   sink(point.x);
        | }
        |""".stripMargin)

    def source = cpg.call.code("source.*")
    def sink   = cpg.call.code("sink.*").argument
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe Set(
      List(("source()", 16), ("k = source()", 16), ("point.x = k", 17), ("sink(point.x)", 19))
    )

  }

  "Flow for object element access" in {
    val cpg: Cpg = code("""
        | var s = { 'field' : 0 }
        |
        | function foo(arg) {
        |   arg.field = source();
        |   sink(arg.field);
        | }
        |""".stripMargin)

    def source = cpg.call.code("source.*")
    def sink   = cpg.call.code("sink.*").argument
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(List(("source()", 5), ("arg.field = source()", 5), ("sink(arg.field)", 6)))
  }

  "Flow for object element access passed to source" in {
    val cpg: Cpg = code("""
        |function bar() {
        |  source(a.b);
        |  sink(a.b);
        |}
        |
        |""".stripMargin)

    def source = cpg.call.code("source.*").argument
    def sink   = cpg.call.code("sink.*").argument
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe Set(List(("source(a.b)", 3), ("sink(a.b)", 4)))
  }

  "Flows for statements to METHOD_RETURN" in {
    val cpg: Cpg = code("""
        |function foo(y, x) {
        |  free(y);
        |  free(x);
        |}
        |""".stripMargin)

    def source = cpg.call.code("free.*").argument(1)
    def sink   = cpg.method(".*foo").methodReturn
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(List(("free(y)", 3), ("RET", 2)), List(("free(x)", 4), ("RET", 2)))
  }

  "Should not create edges from call to ret twice" in {
    val cpg: Cpg = code("""
        | function foo() {
        |   return bar();
        | }
    """.stripMargin)

    cpg.call
      .code("bar.*")
      .outE(EdgeTypes.REACHING_DEF)
      .count(_.inNode() == cpg.ret.head) shouldBe 1
  }

  "Flow from outer params to inner params" in {
    val cpg: Cpg = code("""
        | function f(x, y) {
        |   g(x, y);
        | }""".stripMargin)

    def source = cpg.method(".*f").parameter
    def sink   = cpg.call.code("g.*").argument
    def flows  = sink.reachableByFlows(source)

    sink.size shouldBe 3   // incl. this
    source.size shouldBe 3 // incl. this

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(List(("f(this, x, y)", 2), ("g(x, y)", 3)))
  }

}
