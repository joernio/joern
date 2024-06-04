package io.joern.jssrc2cpg.dataflow

import io.joern.dataflowengineoss.language._
import io.joern.jssrc2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.CfgNode
import io.shiftleft.semanticcpg.language._

class DataflowTests extends DataFlowCodeToCpgSuite {

  "Flow from function call read to multiple versions of the same variable" in {
    val cpg: Cpg = code("""
      |function flows1(fd, mode) {
      |  var buff = [];
      |
      |  var sz = 0;
      |  if (mode == 1) sz = 20;
      |  if (mode == 2) sz = 200;
      |  if (mode == 3) sz = 41;
      |  if (mode == 5) sz = -5;
      |
      |  read(fd, buff, sz);
      |}""".stripMargin)

    def source = cpg.identifier.name("sz")
    def sink   = cpg.call.code("read.*")
    def flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(
        List(("read(fd, buff, sz)", 11)),
        List(("var sz = 0", 5), ("read(fd, buff, sz)", 11)),
        List(("sz = 20", 6), ("read(fd, buff, sz)", 11)),
        List(("sz = 200", 7), ("read(fd, buff, sz)", 11)),
        List(("sz = 41", 8), ("read(fd, buff, sz)", 11)),
        List(("sz = -5", 9), ("read(fd, buff, sz)", 11))
      )

    val tmpSourceFile = flows.head.elements.head.asInstanceOf[CfgNode].method.filename
    val flowsPretty   = flows.p.mkString
    flowsPretty should (include("sz = 20") and include("read(fd, buff, sz)"))
    flowsPretty should include(tmpSourceFile)
  }

  "Flow from function call argument" in {
    val cpg: Cpg = code("""
        |function foo(x) {}
        |
        |function method(y) {
        |  var a = 10;
        |  if (a < y) {
        |    foo(a);
        |  }
        |}""".stripMargin)

    val source = cpg.identifier.name("a")
    val sink   = cpg.call.code("foo.*").argument
    val flows  = sink.reachableByFlows(source)
    flows.size shouldBe 6
  }

  "Flow chains from x to a" in {
    val cpg: Cpg = code("""
      |function flow() {
      |  var a = 0x37;
      |  var b = a;
      |  var c = 0x31;
      |  var z = b + c;
      |  z++;
      |  var p = z;
      |  var x = z;
      |}""".stripMargin)

    val source = cpg.identifier.name("a")
    val sink   = cpg.identifier.name("x")
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(
        List(("var a = 0x37", 3), ("var b = a", 4), ("b + c", 6), ("var z = b + c", 6), ("z++", 7), ("var x = z", 9)),
        List(("var b = a", 4), ("b + c", 6), ("var z = b + c", 6), ("z++", 7), ("var x = z", 9))
      )
  }

  "Flow from method return to a" in {
    val cpg: Cpg = code("""
      |function flow(a) {
      |  var z = a;
      |  var b = z;
      |
      |  return b;
      |}""".stripMargin)

    val source = cpg.identifier.name("a")
    val sink   = cpg.method(".*flow").ast.isReturn
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(List(("var z = a", 3), ("var b = z", 4), ("return b", 6)))
  }

  "Flow with nested if-statements from method return to a" in {
    val cpg: Cpg = code("""
      |function nested(a) {
      |  var x = 0;
      |  var z = 1;
      |  if(a < 10) {
      |    if( a < 5) {
      |      if(a < 2) {
      |        x = a;
      |      }
      |    }
      |  } else
      |    x = z;
      |
      |  return x;
      |}""".stripMargin)

    val source = cpg.call.code("a < 10").argument.code("a")
    val sink   = cpg.method(".*nested").ast.isReturn
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(List(("a < 10", 5), ("a < 5", 6), ("a < 2", 7), ("x = a", 8), ("return x", 14)))
  }

  "Flow with nested if-statements to `return x`" in {
    val cpg: Cpg = code("""
      |function nested(a) {
      |  var x = 0;
      |  var z = 1;
      |  if(a < 10) {
      |    if( a < 5) {
      |      if(a < 2) {
      |        x = a;
      |      }
      |    }
      |  } else
      |    x = z;
      |
      |  return x;
      |}""".stripMargin)

    val source = cpg.identifier.name("x")
    val sink   = cpg.method(".*nested").ast.isReturn
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(
        List(("return x", 14)),
        List(("x = z", 12), ("return x", 14)),
        List(("var x = 0", 3), ("return x", 14)),
        List(("x = a", 8), ("return x", 14))
      )
  }

  "Flow chain from function argument of foo to a" in {
    val cpg: Cpg = code("""
      |function foo(y) {};
      |
      |function param(x) {
      |  var a = x;
      |  var b = a;
      |  var z = foo(b);
      |} """.stripMargin)

    val source = cpg.identifier.name("a")
    val sink   = cpg.call.code("foo.*").argument
    val flows  = sink.reachableByFlows(source)
    flows.size shouldBe 4
  }

  "Flow from function foo to a" in {
    val cpg: Cpg = code("""
      |function param(x) {
      |  var a = x;
      |  var b = a;
      |  var z = foo(b);
      |}""".stripMargin)

    val source = cpg.identifier.name("a")
    val sink   = cpg.call.code("foo.*").argument(1)
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(List(("var a = x", 3), ("var b = a", 4), ("foo(b)", 5)), List(("var b = a", 4), ("foo(b)", 5)))
  }

  "Flow with member access in expression to identifier x" in {
    val cpg: Cpg = code("""
      |var node = {
      |  'value1' : 1,
      |  'value2' : 2
      |};
      |
      |function test() {
      |  var x = 10;
      |  node.value1 = x;
      |  node.value2 = node.value1;
      |}""".stripMargin)

    val source = cpg.identifier.name("x")
    val sink   = cpg.call.code("node.value2")
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(
        List(("node.value1 = x", 9), ("node.value2 = node.value1", 10)),
        List(("var x = 10", 8), ("node.value1 = x", 9), ("node.value2 = node.value1", 10))
      )
  }

  "Flow chain from x to literal 37" in {
    val cpg: Cpg = code("""
      |function flow() {
      |  var a = 37;
      |  var b = a;
      |  var c = 31;
      |  var z = b + c;
      |  z++;
      |  var p = z;
      |  var x = z;
      |}""".stripMargin)

    val source = cpg.literal.code("37")
    val sink   = cpg.identifier.name("x")
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(List(("var a = 37", 3), ("var b = a", 4), ("b + c", 6), ("var z = b + c", 6), ("z++", 7), ("var x = z", 9)))
  }

  "Flow with short hand assignment operator" in {
    val cpg: Cpg = code("""
      |function flow() {
      |  var a = 37;
      |  var b = a;
      |  var z = b;
      |  z += a;
      |}""".stripMargin)

    val source = cpg.call.code("var a = 37").argument(2)
    val sink   = cpg.call.code("z \\+= a").argument(1)
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(List(("var a = 37", 3), ("var b = a", 4), ("var z = b", 5), ("z += a", 6)))
  }

  "Flow after short hand assignment" in {
    val cpg: Cpg = code("""
      |function flow() {
      |  var a = 37;
      |  var b = a;
      |  var z = b;
      |  z += a;
      |  var w = z;
      |}""".stripMargin)

    val source = cpg.call.code("var a = 37").argument(1)
    val sink   = cpg.identifier.name("w")
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(List(("var a = 37", 3), ("var b = a", 4), ("var z = b", 5), ("z += a", 6), ("var w = z", 7)))
  }

  "Flow from array method parameter to identifier" in {
    val cpg: Cpg = code("""
        |function main(argc, argv){
        |  var x = argv[1];
        |  var y = x;
        |  var z = y;
        |  return 0;
        |}""".stripMargin)

    val source = cpg.method(".*main").parameter
    val sink   = cpg.identifier.name("y")
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(
        List(("main(this, argc, argv)", 2), ("var x = argv[1]", 3), ("var y = x", 4), ("var z = y", 5)),
        List(("main(this, argc, argv)", 2), ("var x = argv[1]", 3), ("var y = x", 4))
      )
  }

  "Flow for conditional expressions" in {
    val cpg: Cpg = code("""
      |function foo(x, y) {
      |  var z =  x ? f(y) : g(y);
      |  return;
      |}""".stripMargin)

    val source = cpg.method.parameter.name("y")
    val sink   = cpg.identifier.name("z")
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe Set(
      List(("foo(this, x, y)", 2), ("f(y)", 3), ("x ? f(y) : g(y)", 3), ("var z =  x ? f(y) : g(y)", 3))
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

    val source = cpg.call.code("source.*")
    val sink   = cpg.call.code("sink.*").argument
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe Set(
      List(("source()", 3), ("var x = source()", 3), ("foo(x)", 4), ("foo(this, y)", 7), ("sink(y)", 8))
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

    val source = cpg.call.code("source.*")
    val sink   = cpg.call.code("sink.*").argument(1)
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe Set(
      List(("source()", 3), ("return source()", 3), ("RET", 2), ("bar()", 9), ("var y = bar()", 9), ("sink(y)", 10))
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

    val source = cpg.call.code("source.*")
    val sink   = cpg.method(".*sink").parameter.index(1)
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe Set(
      List(
        ("source()", 3),
        ("return source()", 3),
        ("RET", 2),
        ("bar()", 9),
        ("var y = bar()", 9),
        ("sink(y)", 10),
        ("sink(this, param)", 6)
      )
    )
  }

  "Flow for struct data" in {
    val cpg: Cpg = code("""
      |var point = {
      |  'x' : 0,
      |  'y' : 0
      |}
      |
      |function source() {
      |  return 2.0;
      |}
      |
      |function sink(x) {
      |  return 3;
      |}
      |
      |function main() {
      |  var k = source();
      |  point.x = k;
      |  point.y = 2;
      |  sink(point.x);
      |}""".stripMargin)

    val source = cpg.call.code("source.*")
    val sink   = cpg.call.code("sink.*").argument
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe Set(
      List(("source()", 16), ("var k = source()", 16), ("point.x = k", 17), ("sink(point.x)", 19))
    )

  }

  "Flow for object element access" in {
    val cpg: Cpg = code("""
      |var s = { 'field' : 0 }
      |
      |function foo(arg) {
      |  arg.field = source();
      |  sink(arg.field);
      |}""".stripMargin)

    val source = cpg.call.code("source.*")
    val sink   = cpg.call.code("sink.*").argument
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(List(("source()", 5), ("arg.field = source()", 5), ("sink(arg.field)", 6)))
  }

  "Flow for object element access passed to source" in {
    val cpg: Cpg = code("""
      |function bar() {
      |  source(a.b);
      |  sink(a.b);
      |}""".stripMargin)

    val source = cpg.call.code("source.*").argument
    val sink   = cpg.call.code("sink.*").argument
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe Set(List(("source(a.b)", 3), ("sink(a.b)", 4)))
  }

  "Flows for statements to METHOD_RETURN" in {
    val cpg: Cpg = code("""
      |function foo(y, x) {
      |  free(y);
      |  free(x);
      |}""".stripMargin)

    val source = cpg.call.code("free.*").argument(1)
    val sink   = cpg.method(".*foo").methodReturn
    val flows  = sink.reachableByFlows(source)

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(List(("free(y)", 3), ("RET", 2)), List(("free(x)", 4), ("RET", 2)))
  }

  "Should not create edges from call to ret twice" in {
    val cpg: Cpg = code("""
      |function foo() {
      |  return bar();
      |}""".stripMargin)

    cpg.call
      .code("bar.*")
      .outE(EdgeTypes.REACHING_DEF)
      .count(_.inNode() == cpg.ret.head) shouldBe 1
  }

  "Flow from outer params to inner params" in {
    val cpg: Cpg = code("""
      |function f(x, y) {
      |  g(x, y);
      |}""".stripMargin)

    def source = cpg.method(".*f").parameter
    def sink   = cpg.call.code("g.*").argument
    def flows  = sink.reachableByFlows(source)

    sink.size shouldBe 3   // incl. this
    source.size shouldBe 3 // incl. this

    flows.map(flowToResultPairs).toSetMutable shouldBe
      Set(List(("f(this, x, y)", 2), ("g(x, y)", 3)))
  }

  "Flow from non-static member to sink" in {
    val cpg: Cpg = code("""
      |class Foo {
      |  x = "foo";
      |  func() {
      |    sink(x);
      |  }
      |}
      |""".stripMargin)

    val sink   = cpg.call("sink").argument(1).l
    val source = cpg.member.name("x").l
    sink.size shouldBe 1
    source.size shouldBe 1
    sink.reachableBy(source).size shouldBe 1
  }

  "Flow from static member to sink" in {
    val cpg: Cpg = code("""
      |class Foo {
      |  static x = "foo";
      |  func() {
      |    sink(x);
      |  }
      |}
      |""".stripMargin)

    val sink   = cpg.call("sink").argument(1).l
    val source = cpg.member.name("x").l
    sink.size shouldBe 1
    source.size shouldBe 1
    sink.reachableBy(source).size shouldBe 1
  }

  "Flow from receiver to closure parameters" in {
    val cpg: Cpg = code("foo.bar( (x,y) => { sink1(x); sink2(y); } )")

    val sink = cpg.call("sink1").argument(1).l
    val src  = cpg.identifier.name("foo").l
    sink.reachableBy(src).size shouldBe 1
  }

  "Flow through constructor" in {
    val cpg: Cpg = code("const x = new Foo(y);")

    val sink = cpg.identifier("x").l
    val src  = cpg.identifier("y").l
    sink.reachableBy(src).size shouldBe 1
  }

  "Flow through constructor and object notation" in {
    val cpg: Cpg = code("const x = new Foo({ z : y } );")

    val sink = cpg.identifier("x").l
    val src  = cpg.identifier("y").l
    sink.reachableBy(src).size shouldBe 1
  }

  "Flow from field via object notation" in {
    val cpg: Cpg = code("const x = { p : a.y };")

    val sink = cpg.identifier("x").l
    val src  = cpg.fieldAccess.where(_.fieldIdentifier.canonicalName("y")).l
    src.size shouldBe 1
    sink.size shouldBe 1
    sink.reachableBy(src).size shouldBe 1
  }

  "Flow from inside object notation to call argument" in {
    val cpg: Cpg = code("""
      |const a = { b : 47 } ;
      |fn(a);
      |""".stripMargin)

    val sink = cpg.call.nameExact("fn")
    val src  = cpg.literal("47")
    // Deduplicated as flows skip over certain lowerings in other variants of the flows but source-sink pairs are equal
    sink.reachableBy(src).dedup.size shouldBe 1
  }

  "Flow into method defined as lambda and assigned to constant" in {
    val cpg: Cpg = code("""
      |const foo = (x, y) => {
      |  sink(x);
      |};
      |foo(1, 2);
      |""".stripMargin)

    val sink = cpg.call("sink").l
    val src  = cpg.literal.code("1").l
    sink.size shouldBe 1
    src.size shouldBe 1
    sink.reachableBy(src).size shouldBe 1
  }

  "Should not reach irrelevant nodes" in {
    val cpg: Cpg = code("""
      |const irrelevant = "irrelevant";
      |const a = { } ;
      |sink(a);""".stripMargin)

    val sink = cpg.call("sink").l
    val src  = cpg.literal("\"irrelevant\"").l
    sink.size shouldBe 1
    src.size shouldBe 1
    sink.reachableBy(src).size shouldBe 0
  }

  "Flow correctly through for-in loops" in {
    val cpg: Cpg = code("""
      |function foo(x) {
      |  for(var elem in x) {
      |    console.log(elem)
      |  }
      |}""".stripMargin)

    val sink = cpg.identifier("elem").l
    val src1 = cpg.method("foo").parameter.order(1).l
    val src2 = cpg.identifier("x").l

    // two flows because we find a second one in the lowered for-in loop code
    sink.reachableBy(src1).size shouldBe 2
    sink.reachableBy(src2).size shouldBe 2
  }

  "Flow correctly through for-each lambda" in {
    val cpg: Cpg = code("""
      |function foo(x) {
      |  Object.keys(x).forEach(elem => console.log(elem))
      |}""".stripMargin)

    val sink = cpg.identifier("elem").l
    val src1 = cpg.method("foo").parameter.order(1).l
    val src2 = cpg.identifier("x").l
    sink.reachableBy(src1).size shouldBe 1
    sink.reachableBy(src2).size shouldBe 1
  }

  "Flow correctly from parent scope to child function scope" in {
    val cpg: Cpg = code("""
        |function foo(u) {
        |
        |  const x = 1;
        |
        |  function bar() {
        |     y = x;
        |     console.log(y);
        |     v = u;
        |     console.debug(v);
        |  }
        |
        |}""".stripMargin)

    val sink1 = cpg.call("log").l
    val sink2 = cpg.call("debug").l
    sink1.size shouldBe 1
    sink2.size shouldBe 1

    val iSrc = cpg.method("foo").ast.isIdentifier.name("x").lineNumber(4).l
    iSrc.size shouldBe 1
    sink1.reachableBy(iSrc).dedup.size shouldBe 1

    val lSrc = cpg.method("foo").ast.isLiteral.code("1").lineNumber(4).l
    lSrc.size shouldBe 1
    sink1.reachableBy(lSrc).size shouldBe 1

    val pSrc = cpg.method("foo").parameter.nameExact("u").l
    pSrc.size shouldBe 1
    sink2.reachableBy(pSrc).size shouldBe 1
  }

  "Flow of multiple assignment" in {
    val cpg: Cpg = code("""
        |const middle = source()
        |const number = 1, sink = middle.fn()
        |""".stripMargin)

    def src = cpg.call("source")
    def snk = cpg.identifier("sink")
    snk.reachableByFlows(src) should have size 1
  }

  "Flow from a module-level literal to a call captured by a closure" should {
    val cpg = code("""
        |import axios from 'axios';
        |import { User } from './user';
        |
        |const API_Endpoint = "https://test-api-service.com";
        |
        |export const createUser = (user: User) => {
        |  return axios.post(API_Endpoint + "/user", user);
        |};
        |""".stripMargin)

    val sink = cpg.call.code("axios.post\\(.*").l

    "literal to captured closure" in {
      val literalSource = cpg.literal.codeExact("\"https://test-api-service.com\"").l
      literalSource.size shouldBe 1
      sink.reachableBy(literalSource).dedup.size shouldBe 1
    }

    "identifiers to captured closure" in {
      val identifierSource = cpg.identifier.nameExact("API_Endpoint").lineNumber(5).l
      identifierSource.size shouldBe 1
      sink.reachableBy(identifierSource).dedup.size shouldBe 1
    }

    "identifiers in the arg of the call" in {
      val identifierSource = cpg.identifier.nameExact("API_Endpoint").lineNumber(8).l
      identifierSource.size shouldBe 1
      sink.reachableBy(identifierSource).size shouldBe 1
    }
  }

  "Field access on TemplatedDom directly" should {
    val cpg = code("""
     |import { useRouter } from "next/router";
     |const tabComponentType = (<Tab title={"typeComponent"} />).type;
     |""".stripMargin)
    "Not throw error and get handled it gracefully" in {
      val List(x) = cpg.identifier("tabComponentType").l
      x.lineNumber shouldBe Option(3)
    }
  }
}
