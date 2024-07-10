package io.joern.swiftsrc2cpg.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.queryengine.EngineConfig
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.swiftsrc2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.Identifier
import io.shiftleft.codepropertygraph.generated.nodes.Literal
import io.shiftleft.semanticcpg.language.*

class DataFlowTests extends DataFlowCodeToCpgSuite {

  "DataFlowTest1" should {
    val cpg = code("""
        |struct node {
        |  var value: Int
        |  var next: node
        |}
        |
        |func free_list(head: node) {
        |  var q: node
        |  var p: node = head
        |  while p != null {
        |    p = q
        |    q = p.next
        |    free(elem: p)
        |  }
        |}
        |
        |func flow(p0: Int) -> Int {
        |  var a: Int = p0
        |  var b: Int = a
        |  var c: Int = 0x31
        |  var z: Int = b + c
        |  z++
        |  var x: Int = z
        |  return x
        |}""".stripMargin)

    "identify all calls to `free`" in {
      cpg.call.name("free").code.toSetMutable shouldBe Set("free(elem: p)")
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
      sink.reachableByFlows(source).l.map(flowToResultPairs).distinct.size shouldBe 8
    }

    "find flows from z to method returns of flow" in {
      val source = cpg.identifier.name("z")
      val sink   = cpg.method.name("flow").methodReturn
      sink.reachableByFlows(source).size shouldBe 3
    }
  }

  "DataFlowTest2" should {
    val cpg = code("""
      |func foo(x: Int) -> Int {
      |  return x
      |}""".stripMargin)

    "Test ParameterToReturn1" should {
      "have a flow from input parameter to return" in {
        val source = cpg.method.name("foo").parameter.name("x")
        val sink   = cpg.method.name("foo").methodReturn
        val flows  = sink.reachableByFlows(source)

        flows.map(flowToResultPairs).toSetMutable shouldBe
          Set(List(("foo(x: Int)", 2), ("return x", 3), ("RET", 2)))
      }
    }

  }

  "DataFlowTest3" should {
    val cpg = code("""
      |func foo(x: Int) -> Int {
      |  var k = x + 1
      |  var y = k + 2
      |  return y + 3
      |}""".stripMargin)

    "Test ParameterToReturn2" should {
      "have a flow from input parameter to return" in {
        val source = cpg.method.name("foo").parameter.name("x")
        val sink   = cpg.method.name("foo").methodReturn
        val flows  = sink.reachableByFlows(source)

        flows.map(flowToResultPairs).toSetMutable shouldBe
          Set(
            List(
              ("foo(x: Int)", 2),
              ("x + 1", 3),
              ("var k = x + 1", 3),
              ("k + 2", 4),
              ("var y = k + 2", 4),
              ("y + 3", 5),
              ("return y + 3", 5),
              ("RET", 2)
            )
          )
      }
    }
  }

  "DataFlowTest4" should {
    val cpg = code("""
      |struct Point {
      |  var x: Int
      |  var y: Int
      |};
      |
      |func source() -> Double {
      |  return 2.0
      |}
      |
      |func sink(x: Int) -> Int {
      |  return 3
      |}
      |
      |func main() {
      |  var k: Int = source(2)
      |  var point: Point
      |  point.x = k
      |  point.y = 2
      |  sink(point.x)
      |}""".stripMargin)

    "Test StructDataFlow" should {
      "have a flow from input parameter to return" in {
        val source = cpg.method.name("source").methodReturn
        val sink   = cpg.method.name("sink").parameter.name("x")
        val flows  = sink.reachableByFlows(source)

        flows.map(flowToResultPairs).toSetMutable shouldBe
          Set(
            List(
              ("RET", 7),
              ("source(2)", 16),
              ("var k: Int = source(2)", 16),
              ("point.x = k", 18),
              ("sink(point.x)", 20),
              ("sink(x: Int)", 11)
            )
          )
      }
    }

  }

  "DataFlowTest5" should {
    val cpg = code("""
      |func source() -> Int {
      |  return 2
      |}
      |
      |func sink(x: Int) -> Int {
      |  return 3
      |}
      |
      |func main() {
      |  var k = source()
      |  foo(k)
      |}
      |
      |func foo(par: Int) {
      |  sink(par)
      |}""".stripMargin)

    "Test Interprocedural" should {
      "have a flow from input parameter to return" in {
        val source = cpg.method.name("source").methodReturn
        val sink   = cpg.method.name("sink").parameter.name("x")
        val flows  = sink.reachableByFlows(source)

        flows.map(flowToResultPairs).toSetMutable shouldBe
          Set(
            List(
              ("RET", 2),
              ("source()", 11),
              ("var k = source()", 11),
              ("foo(k)", 12),
              ("foo(par: Int)", 15),
              ("sink(par)", 16),
              ("sink(x: Int)", 6)
            )
          )
      }
    }

  }

  "DataFlowTest6" should {
    val cpg = code("""
      |struct Point {
      |  var x: Int
      |  var y: Int
      |}
      |
      |func source() -> Point {
      |  var point: Point
      |  return point
      |}
      |
      |func sink(x: Int) -> Int {
      |  return 0
      |}
      |
      |func main() {
      |  var point = source(2)
      |  sink(point.x)
      |}""".stripMargin)

    "Test TaintedStruct" should {
      "have a flow from input parameter to return" in {
        val source = cpg.method.name("source").methodReturn
        val sink   = cpg.method.name("sink").parameter.name("x")
        val flows  = sink.reachableByFlows(source)

        flows.map(flowToResultPairs).toSetMutable shouldBe
          Set(
            List(
              ("RET", 7),
              ("source(2)", 17),
              ("var point = source(2)", 17),
              ("sink(point.x)", 18),
              ("sink(x: Int)", 12)
            )
          )
      }
    }

  }

  "DataFlowTest8" should {
    val cpg = code("""
      |func source() -> Int {}
      |func sink(cont: [Int]) {}
      |
      |func foo(c: [[Int]], idx: Int) {
      |  c[1][2] = source()
      |  c[idx][2] = 0
      |  sink(c[1])
      |}""".stripMargin)

    "Exclusions behind over-taint" should {
      "not kill flows" in {
        val source = cpg.method.name("source").methodReturn
        val sink   = cpg.method.name("sink").parameter
        val flows  = sink.reachableByFlows(source)

        flows.map(flowToResultPairs).toSetMutable shouldBe Set(
          List(("RET", 2), ("source()", 6), ("c[1][2] = source()", 6), ("sink(c[1])", 8), ("sink(cont: [Int])", 3))
        )
      }
    }

  }

  "DataFlowTest10" should {
    val cpg = code("""
      |func source() -> Int {}
      |func sink(i: [Int]) {}
      |
      |func foo(arg: [Int]) {
      |  arg[0] = source()
      |  sink(arg)
      |}""".stripMargin)

    "Pointer deref vs array access" should {
      "actually find flows" in {
        val source = cpg.method.name("source").methodReturn
        val sink   = cpg.method.name("sink").parameter
        val flows  = sink.reachableByFlows(source)

        flows.map(flowToResultPairs).toSetMutable shouldBe Set(
          List(("RET", 2), ("source()", 6), ("arg[0] = source()", 6), ("sink(arg)", 7), ("sink(i: [Int])", 3))
        )
      }
    }

  }

  "DataFlowTest11" should {
    val cpg = code("""
      |func main() -> Int {
      |  var a: Int = getpid()
      |  var b: Int = 888
      |  if a == 666 {
      |    a = a * 666
      |    b = 999
      |  } else {
      |    a = a * 777
      |  } 
      |  return a
      |}""".stripMargin)

    "PathUnfolding with allFlows" should {
      "work as expected" in {
        val source = cpg.call("getpid")
        val sink   = cpg.ret
        val flows  = sink.reachableByFlows(source)

        flows.map(flowToResultPairs).toSetMutable shouldBe
          Set(
            List(
              ("getpid()", 3),
              ("var a: Int = getpid()", 3),
              ("a == 666", 5),
              ("a * 666", 6),
              ("a = a * 666", 6),
              ("return a", 11)
            )
          )
      }
    }

  }

  "DataFlowTest13" should {
    val cpg = code("""
      |func flows1(fd: File, mode: Int) {
      |  var buff: [Char]
      |  var sz: Int = 0
      |  if mode == 1 { sz = 20 }
      |  if mode == 2 { sz = 200 }
      |  if mode == 3 { sz = 41 }
      |  if mode == 5 { sz = -5 }
      |  read(fd, buff, sz)
      |}""".stripMargin)

    "flow from function call read to multiple versions of the same variable" in {
      def source = cpg.identifier.name("sz")
      def sink   = cpg.call.name("read")
      def flows  = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSetMutable shouldBe
        Set(
          List(("var sz: Int = 0", 4), ("read(fd, buff, sz)", 9)),
          List(("sz = 200", 6), ("read(fd, buff, sz)", 9)),
          List(("read(fd, buff, sz)", 9)),
          List(("sz = 20", 5), ("read(fd, buff, sz)", 9)),
          List(("sz = 41", 7), ("read(fd, buff, sz)", 9)),
          List(("sz = -5", 8), ("read(fd, buff, sz)", 9))
        )

      val flowsPretty = flows.p.mkString
      flowsPretty should include("sz = 20")
      flowsPretty should include("read(fd, buff, sz)")
    }
  }

  "DataFlowTest15" should {
    val cpg = code("""
      |func method(y: Int) -> Int {
      |  var a: Int = 10
      |  if a < y {
      |    foo(a)
      |  }
      |}""".stripMargin)

    "flow from function call argument" in {
      implicit val callResolver: NoResolve.type = NoResolve

      val source = cpg.identifier.name("a")
      val sink   = cpg.method.name("foo").parameter.argument
      val flows  = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSetMutable shouldBe
        Set(
          List(("a < y", 4), ("foo(a)", 5)),
          List(("var a: Int = 10", 3), ("a < y", 4), ("foo(a)", 5)),
          List(("foo(a)", 5))
        )
    }
  }

  "DataFlowTest16" should {
    val cpg = code("""
      |func flow() {
      |  var a: Int = 0x37
      |  var b: Int = a
      |  var c: Int = 0x31
      |  var z: Int = b + c
      |  z++
      |  var p: Int = z
      |  var x: Int = z
      |}""".stripMargin)

    "flow chains from x to a" in {
      val source = cpg.identifier.name("a")
      val sink   = cpg.identifier.name("x")
      val flows  = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSetMutable shouldBe
        Set(
          List(
            ("var a: Int = 0x37", 3),
            ("var b: Int = a", 4),
            ("b + c", 6),
            ("var z: Int = b + c", 6),
            ("z++", 7),
            ("var x: Int = z", 9)
          ),
          List(("var b: Int = a", 4), ("b + c", 6), ("var z: Int = b + c", 6), ("z++", 7), ("var x: Int = z", 9))
        )
    }
  }

  "DataFlowTest17" should {
    val cpg = code("""
      |func flow(a: Int) -> Int {
      |  var z: Int = a
      |  var b: Int = z
      |  return b
      |}""".stripMargin)

    "flow from method return to a" in {
      val source = cpg.identifier.name("a")
      val sink   = cpg.method("flow").ast.isReturn
      val flows  = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSetMutable shouldBe
        Set(List(("var z: Int = a", 3), ("var b: Int = z", 4), ("return b", 5)))
    }
  }

  "DataFlowTest18" should {
    val cpg = code("""
      |func nested(a: Int) -> Int {
      |  var x: Int
      |  var z: Int = 0x37
      |  if a < 10 {
      |    if a < 5 {
      |      if a < 2 {
      |        x = a
      |      }
      |    }
      |  } else { x = z }
      |  return x
      |}""".stripMargin)

    "flow with nested if-statements from method return to a" in {
      val source = cpg.call.code("a < 10").argument.code("a")
      val sink   = cpg.method("nested").ast.isReturn
      val flows  = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSetMutable shouldBe
        Set(List(("a < 10", 5), ("a < 5", 6), ("a < 2", 7), ("x = a", 8), ("return x", 12)))
    }
  }

  "DataFlowTest19" should {
    val cpg = code("""
      |func nested(a: Int) -> Int {
      |  var x: Int
      |  var z: Int = 0x37
      |  if a < 10 {
      |    if a < 5 {
      |      if a < 2 {
      |        x = a
      |      }
      |    }
      |  } else { x = z }
      |  return x
      |}""".stripMargin)

    "flow with nested if-statements to `return x`" in {
      val source = cpg.identifier.name("x")
      val sink   = cpg.method("nested").ast.isReturn
      val flows  = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSetMutable shouldBe
        Set(List(("x = z", 11), ("return x", 12)), List(("x = a", 8), ("return x", 12)), List(("return x", 12)))
    }
  }

  "DataFlowTest20" should {
    val cpg = code("""
      |func param(x: Int) {
      |  var a: Int = x
      |  var b: Int = a
      |  var z: Int = foo(b)
      |}""".stripMargin)

    "flow chain from function argument of foo to a" in {
      implicit val callResolver: NoResolve.type = NoResolve
      val source                                = cpg.identifier.name("a")
      val sink                                  = cpg.method.name("foo").parameter.argument
      val flows                                 = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSetMutable shouldBe
        Set(
          List(("var b: Int = a", 4), ("foo(b)", 5)),
          List(("var a: Int = x", 3), ("var b: Int = a", 4), ("foo(b)", 5))
        )
    }
  }

  "DataFlowTest21" should {
    val cpg = code("""
     |func param(x: Int) {
     |  var a: Int = x
     |  var b: Int = a
     |  var z: Int = foo(b)
     |}""".stripMargin)

    "flow from function foo to a" in {
      val source = cpg.identifier.name("a")
      val sink   = cpg.call.name("foo").argument(1)
      val flows  = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSetMutable shouldBe
        Set(
          List(("var b: Int = a", 4), ("foo(b)", 5)),
          List(("var a: Int = x", 3), ("var b: Int = a", 4), ("foo(b)", 5))
        )
    }
  }

  "DataFlowTest22" should {
    val cpg = code("""
      |struct node {
      |  var value1: Int
      |  var value2: Int
      |}
      |
      |func test() {
      |  var x: Int = 10
      |  var n: Node
      |  n.value1 = x
      |  n.value2 = n.value1
      |}""".stripMargin)

    "flow with member access in expression to identifier x" in {
      val source = cpg.identifier.name("x")
      val sink   = cpg.call.code("n.value2")
      val flows  = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSetMutable shouldBe
        Set(
          List(("var x: Int = 10", 8), ("n.value1 = x", 10), ("n.value2 = n.value1", 11)),
          List(("n.value1 = x", 10), ("n.value2 = n.value1", 11))
        )
    }
  }

  "DataFlowTest23" should {
    val cpg = code("""
        |func flow() {
        |  var a: Int = 0x37
        |  var b: Int = a
        |  var c: Int = 0x31
        |  var z: Int = b + c
        |  z++
        |  var p: Int = z
        |  var x: Int = z
        |}""".stripMargin)

    "flow chain from x to literal 0x37" in {
      val source = cpg.literal.code("0x37")
      val sink   = cpg.identifier.name("x")
      val flows  = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSetMutable shouldBe
        Set(
          List(
            ("var a: Int = 0x37", 3),
            ("var b: Int = a", 4),
            ("b + c", 6),
            ("var z: Int = b + c", 6),
            ("z++", 7),
            ("var x: Int = z", 9)
          )
        )
    }
  }

  "DataFlowTest24" should {
    val cpg = code("""
      |func flow() {
      |  var a: Int = 0x37
      |  var b: Int = a
      |  var z: Int = b
      |  z += a
      |}""".stripMargin)

    "flow with short hand assignment operator" in {
      val source = cpg.call.codeExact("var a: Int = 0x37").argument(2)
      val sink   = cpg.call.codeExact("z += a").argument(1)
      val flows  = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSetMutable shouldBe
        Set(List(("var a: Int = 0x37", 3), ("var b: Int = a", 4), ("var z: Int = b", 5), ("z += a", 6)))
    }
  }

  "DataFlowTest25" should {
    val cpg = code("""
      |func flow(void) {
      |  var a: Int = 0x37
      |  var b: Int = a
      |  var z: Int = b
      |  z += a;
      |  var w: Int = z
      |}""".stripMargin)

    "flow after short hand assignment" in {
      val source = cpg.call.codeExact("var a: Int = 0x37").argument(1)
      val sink   = cpg.identifier.name("w")
      val flows  = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSetMutable shouldBe
        Set(
          List(
            ("var a: Int = 0x37", 3),
            ("var b: Int = a", 4),
            ("var z: Int = b", 5),
            ("z += a", 6),
            ("var w: Int = z", 7)
          )
        )
    }
  }

  "DataFlowTest26" should {
    val cpg = code("""
      |func main(argc: Int, argv: [[Char]]) -> Int {
      |  var x: Int = argv[1]
      |  var y: Int = x
      |  var z: Int = y
      |  return 0
      |}""".stripMargin)

    "flow from array method parameter to identifier" in {
      val source = cpg.method.parameter
      val sink   = cpg.identifier.name("y")
      val flows  = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSetMutable shouldBe
        Set(
          List(
            ("main(argc: Int, argv: [[Char]])", 2),
            ("var x: Int = argv[1]", 3),
            ("var y: Int = x", 4),
            ("var z: Int = y", 5)
          ),
          List(("main(argc: Int, argv: [[Char]])", 2), ("var x: Int = argv[1]", 3), ("var y: Int = x", 4))
        )
    }
  }

  "DataFlowTest27" should {
    val cpg = code("""
      |func foo(x: Bool, y: Int) {
      |  var z =  x ? f(y) : g(y)
      |  return
      |}""".stripMargin)

    "conditional expressions (joern issue #91)" in {
      val source = cpg.method.parameter.name("y")
      val sink   = cpg.identifier.name("z")
      val flows  = sink.reachableByFlows(source)
      flows.size shouldBe 1
    }
  }

  "DataFlowTest28" should {
    val cpg = code("""
      |func bar() -> Int {
      | var x: Int = source()
      | foo(y: x)
      |}
      |
      |func foo(y: Int) {
      |  sink(y)
      |}""".stripMargin)

    "find source in caller" in {
      val source = cpg.call("source")
      val sink   = cpg.call("sink").argument(1)
      sink.reachableByFlows(source).map(flowToResultPairs).toSetMutable shouldBe Set(
        List(("source()", 3), ("var x: Int = source()", 3), ("y: x", 4), ("foo(y: Int)", 7), ("sink(y)", 8))
      )
    }
  }

  "DataFlowTest29" should {
    val cpg = code("""
      |func bar() -> Int {
      |  return source()
      |}
      |
      |func foo(y: Int) {
      |  var y: Int = bar()
      |  sink(y)
      |}""".stripMargin)

    "find source in callee" in {
      val source = cpg.call("source")
      val sink   = cpg.call("sink").argument(1)
      sink.reachableByFlows(source).map(flowToResultPairs).toSetMutable shouldBe
        Set(
          List(
            ("source()", 3),
            ("return source()", 3),
            ("RET", 2),
            ("bar()", 7),
            ("var y: Int = bar()", 7),
            ("sink(y)", 8)
          )
        )
    }

    "allow using formal parameters as sink" in {
      val source = cpg.call("source")
      val sink   = cpg.method("sink").parameter.index(1)
      sink.reachableByFlows(source).map(flowToResultPairs).toSetMutable shouldBe Set(
        List(
          ("source()", 3),
          ("return source()", 3),
          ("RET", 2),
          ("bar()", 7),
          ("var y: Int = bar()", 7),
          ("sink(y)", 8),
          ("sink(p0, p1, p2)", -1)
        )
      )
    }
  }

  "DataFlowTest31" should {
    val cpg = code("""
      |struct Point {
      |  var x: Int
      |  var y: Int
      |}
      |
      |func source() -> Point {
      |  var point: Point
      |  return point
      |}
      |
      |func sink(x: Int) -> Int {
      |  return 0
      |}
      |
      |func main() {
      |  var point: Point = source()
      |  sink(point.x)
      |}""".stripMargin)

    "tainted struct" in {
      val source = cpg.method.name("source").methodReturn
      val sink   = cpg.method.name("sink").parameter.name("x")
      val flows  = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSetMutable shouldBe Set(
        List(
          ("RET", 7),
          ("source()", 17),
          ("var point: Point = source()", 17),
          ("sink(point.x)", 18),
          ("sink(x: Int)", 12)
        )
      )
    }
  }

  "DataFlowTest35" should {
    val cpg = code("""
      |func source() -> Int {}
      |func sink(i: [Int]) {}
      |
      |func foo(arg: [Int]) {
      |  arg[0] = source()
      |  sink(arg)
      |}""".stripMargin)

    "handle array access correctly" in {
      val source = cpg.method.name("source").methodReturn
      val sink   = cpg.call.codeExact("sink(arg)")
      val flows  = sink.reachableByFlows(source)
      flows.size shouldBe 1
    }

  }

  "DataFlowTest38" should {
    val cpg = code("""
      |func foo() {
      |  a.b = source()
      |  a.b = 10
      |  sink(a.b)
      |}""".stripMargin)

    "not report flow" in {
      val source = cpg.call.name("source")
      val sink   = cpg.method.name("sink").parameter
      val flows  = sink.reachableByFlows(source)
      flows.size shouldBe 0
    }
  }

  "DataFlowTest40" should {
    val cpg = code("""
      |func foo() {
      |  var y: Int = 1
      |  y = something_else
      |  y = 10
      |}""".stripMargin)

    "find that there is a flow from `y = 1` to exit node" in {
      // This one may be a bit surprising, but what's happening here
      // is that the expression "y = 1" flows to the exit node, and
      // since that is influenced by 1, there is in fact a flow from
      // "1" to the exit node.
      val source = cpg.literal("1")
      val sink   = cpg.method("foo").methodReturn
      val flows  = sink.reachableByFlows(source)
      flows.size shouldBe 1
    }
  }

  "DataFlowTest42" should {
    val cpg = code("""
      |func foo(b: Int) {
      |  b = source()
      |  b = 10
      |  sink(b)
      |}""".stripMargin)

    "block flow even if variable decl cannot be found" in {
      val source = cpg.call.name("source")
      val sink   = cpg.method.name("sink").parameter
      val flows  = sink.reachableByFlows(source)
      flows.size shouldBe 0

      val source2 = cpg.assignment.codeExact("b = 10").target
      val sink2   = cpg.method.name("sink").parameter
      val flows2  = sink2.reachableByFlows(source2)
      flows2.map(flowToResultPairs).toSetMutable shouldBe Set(
        List(("b = 10", 4), ("sink(b)", 5), ("sink(p0, p1, p2)", -1))
      )
    }
  }

  "DataFlowTest43" should {
    val cpg = code("""
      |func foo() -> Int {
      |  return bar()
      |}""".stripMargin)

    "not create edges from call to ret twice" in {
      cpg
        .call("bar")
        .outE(EdgeTypes.REACHING_DEF)
        .count(_.inNode() == cpg.ret.head) shouldBe 1
    }
  }

  "DataFlowTest44" should {
    val cpg = code("""
      |func f(x: Int, y: Int) {
      |  g(x, y)
      |}""".stripMargin)

    "find flow from outer params to inner params" in {
      val source = cpg.method.name("f").parameter
      val sink   = cpg.method.name("g").parameter
      sink.reachableBy(source).size shouldBe 4
    }
  }

  "DataFlowTest45" should {
    val cpg = code("""
      |func bar(z: Int) -> Int {
      |  var x: Int = 10
      |  var y: Int = x + source()
      |  return y
      |}
      |
      |func foo() -> Int {
      |  var y: Int = bar(x)
      |  sink(y)
      |}""".stripMargin)

    "provide correct flow for source in sibling callee" in {
      cpg.call("sink").argument(1).reachableByFlows(cpg.call("source")).size shouldBe 1
    }

  }

  "DataFlowTest46" should {
    val cpg = code("""
      |func foo() {
      |  var x: Int = source()
      |  sink(x)
      |}""".stripMargin)

    "find flow via assignment" in {
      val source = cpg.call("source")
      val sink   = cpg.call("sink")
      val flows  = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSetMutable shouldBe Set(
        List(("source()", 3), ("var x: Int = source()", 3), ("sink(x)", 4))
      )
    }
  }

  "DataFlowTest47" should {
    val cpg = code("""
      |func sink(arg: Int) -> Int { return arg }
      |func source() -> Int { return 0 }
      |
      |func foo() {
      |  sink(source())
      |}""".stripMargin)

    "find flow of call in call" in {
      val source = cpg.call("source")
      val sink   = cpg.call("sink")
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSetMutable shouldBe Set(
        List(("source()", 6), ("sink(arg: Int)", 2), ("return arg", 2), ("RET", 2), ("sink(source())", 6))
      )
    }
  }

  "DataFlowTest49" should {
    val cpg = code("""
      |func foo(x: Int) {
      |  x = source()
      |  sink(x)
      |}""".stripMargin)

    "find flow via assignment for global" in {
      val source = cpg.call("source")
      val sink   = cpg.call("sink")
      val flows  = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSetMutable shouldBe Set(List(("source()", 3), ("x = source()", 3), ("sink(x)", 4)))
    }
  }

  "DataFlowTest50" should {
    val cpg = code("""
      |func foo() {
      |  var x: Int = source()
      |  x = y
      |  sink(x)
      |}""".stripMargin)

    "find that flow is blocked by assignment" in {
      val source     = cpg.call("source")
      val assignment = cpg.assignment.codeExact("x = y")
      val sink       = cpg.call("sink").l

      val flows = sink.reachableByFlows(source)
      flows.size shouldBe 0

      val flows2 = sink.reachableByFlows(assignment.target)
      flows2.map(flowToResultPairs).toSetMutable shouldBe Set(List(("x = y", 4), ("sink(x)", 5)))
    }
  }

  "DataFlowTest51" should {
    val cpg = code("""
      |func foo() {
      |  x.y = source()
      |  sink(x.y)
      |}""".stripMargin)

    "find via assignment with field access" in {
      val source = cpg.call("source")
      val sink   = cpg.call("sink")
      val flows  = sink.reachableByFlows(source)

      flows.map(flowToResultPairs).toSetMutable shouldBe Set(
        List(("source()", 3), ("x.y = source()", 3), ("sink(x.y)", 4))
      )
    }
  }

  "DataFlowTest53" should {
    val cpg = code("""
      |func foo() {
      |  x.y = source()
      |  x.y = z
      |  sink(x)
      |}""".stripMargin)

    "find that flow is blocked by assignment" in {
      val source     = cpg.call("source")
      def sink       = cpg.call("sink")
      val assignment = cpg.assignment.codeExact("x.y = z")
      val flows      = sink.reachableByFlows(source)

      flows.size shouldBe 0

      val flows2 = sink.reachableByFlows(assignment.target)
      flows2.map(flowToResultPairs).toSetMutable shouldBe Set(List(("x.y = z", 4), ("sink(x)", 5)))
    }
  }

  "DataFlowTest57" should {
    val cpg = code("""
      |func abc() {
      |  var a: Int
      |  a = foo()
      |  a = bar(0x80)
      |  sink(a)
      |}""".stripMargin)

    "not find a flow from 'a' at 'foo' to 'sink'" in {
      val src  = cpg.call("foo").inAssignment.target.head
      val sink = cpg.method("sink").parameter
      sink.reachableByFlows(src).size shouldBe 0
    }
  }

  "DataFlowTest58" should {
    val cpg = code("""
      |func abc(a: Int) {
      |  a = foo()
      |  a = bar(0x80)
      |  sink(a)
      |}""".stripMargin)

    "not find a flow from parameter 'a' to 'sink'" in {
      val src  = cpg.method("abc").parameter
      val sink = cpg.method("sink").parameter
      sink.reachableByFlows(src).size shouldBe 0
    }
  }

  "DataFlowTests60" should {
    val cpg = code("""
      |func outer(ptr: Int) {
      |  taint1(ptr)
      |  inner(ptr)
      |  return
      |}
      |
      |func inner(ptr: Int) {
      |  // taint2(ptr);
      |  ptr = malloc(0x80)
      |  sink(ptr)
      |}""".stripMargin)

    "not return flow" in {
      val source = cpg.call("taint1").argument
      val sink   = cpg.call("sink").argument
      sink.reachableByFlows(source).size shouldBe 0
    }
  }

  "DataFlowTests66" should {
    val cpg = code("""
      |func foo(x: Int) {
      |  x = 10
      |}""".stripMargin)

    "report flow from method assignment to method parameter out" in {
      val sink   = cpg.method("foo").parameter.asOutput
      val source = cpg.method("foo").ast.isIdentifier.name("x")
      sink.reachableByFlows(source).size shouldBe 1
    }

  }

  "DataFlowTests69" should {
    val cpg = code("""
     |func foo() -> String {
     |  return "abc" + "firstName"
     |}
     |
     |func bar() {
     |  log(foo())
     |}
     |""".stripMargin)

    "find a flow where the first element is a literal" in {
      val source           = cpg.literal.code(".*firstName.*")
      val sink             = cpg.call.methodFullName(".*log.*").l
      val List(flow)       = sink.reachableByFlows(source).l
      val literal: Literal = flow.elements.head.asInstanceOf[Literal]
      literal.code shouldBe "\"firstName\""
    }
  }

  "DataFlowTest70" should {
    val cpg = code("""
      |func source() -> Int {
      |  return 42
      |}
      |
      |func main() {
      |  sink(source())
      |}""".stripMargin)

    "Test Interprocedural" should {
      "have a flow from argument(which itself is a call) to return" in {
        val source = cpg.literal("42")
        val sink   = cpg.call("sink").argument
        val flows  = sink.reachableByFlows(source)
        flows.map(flowToResultPairs).toSetMutable shouldBe Set(
          List(("return 42", 3), ("RET", 2), ("source()", 7)),
          List(("return 42", 3), ("RET", 2), ("source()", 7), ("sink(source())", 7))
        )
      }
    }
  }

}

class DataFlowTestsWithCallDepth extends DataFlowCodeToCpgSuite {

  override implicit val context: EngineContext = EngineContext(config = EngineConfig(maxCallDepth = -1))

  "DataFlowTest69" should {
    val cpg = code("""
      |func sink(a: Int) {}
      |
      |func foo() {
      |  var val = 42
      |  var a = 0
      |  var b = 0
      |  a = b = val
      |  sink(a: a)
      |}
      |
      |func bar() {
      |  var val = 42
      |  var a = 0
      |  a = val++
      |  sink(a: a)
      |}""".stripMargin)

    "find flows" in {
      val sink = cpg.method("sink").parameter.index(1).l
      val src  = cpg.literal.l
      sink.reachableBy(src).method.name.toSet shouldBe Set("foo", "bar")
    }
  }

  "DataFlowTest70" should {
    val cpg = code("""
        |func foo() {
        |  var v1: Int = 0
        |  var v2: Int = 0
        |  if (v1 = 1, v2 == 2) || v2 <= 3 return v2
        |  return 0
        |}
        |""".stripMargin)

    "find flows" in {
      val source = cpg.identifier("v2").l
      val sink   = cpg.method("foo").methodReturn.l
      sink.reachableByFlows(source).size shouldBe 4
    }
  }

}
