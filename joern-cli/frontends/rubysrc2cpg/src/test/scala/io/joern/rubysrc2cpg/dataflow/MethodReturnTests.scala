package io.joern.rubysrc2cpg.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class MethodReturnTests extends RubyCode2CpgFixture(withPostProcessing = true, withDataFlow = true) {

  "flow from method parameter to explicit return of the same variable" in {
    val cpg = code("""
        |def f(x)
        | return x
        |end
        |""".stripMargin)
    val source = cpg.method.name("f").parameter.index(1)
    val sink   = cpg.method.name("f").methodReturn
    val flows  = sink.reachableByFlows(source)
    flows.map(flowToResultPairs).toSet shouldBe
      Set(List(("f(self, x)", 2), ("return x", 3), ("RET", 2)))
  }

  "flow from method parameter to implicit return of the same variable" in {
    val cpg = code("""
        |def f(x)
        | x
        |end
        |""".stripMargin)
    val source = cpg.method.name("f").parameter.index(1)
    val sink   = cpg.method.name("f").methodReturn
    val flows  = sink.reachableByFlows(source)
    flows.map(flowToResultPairs).toSet shouldBe
      Set(List(("f(self, x)", 2), ("x", 3), ("RET", 2)))
  }

  "flow from endless method parameter to implicit return of the same variable" in {
    val cpg = code("""
        |def f(x) = x
        |""".stripMargin)
    val source = cpg.method.name("f").parameter.index(1)
    val sink   = cpg.method.name("f").methodReturn
    val flows  = sink.reachableByFlows(source)
    flows.map(flowToResultPairs).toSet shouldBe
      Set(List(("f(self, x)", 2), ("x", 2), ("RET", 2)))
  }

  "flow from method parameter to implicit return via assignment to temporary variable" in {
    val cpg = code("""
        |def f(x)
        | y = x
        |end
        |""".stripMargin)
    val source = cpg.method.name("f").parameter.index(1)
    val sink   = cpg.method.name("f").methodReturn
    val flows  = sink.reachableByFlows(source)
    flows.map(flowToResultPairs).toSet shouldBe
      Set(List(("f(self, x)", 2), ("y = x", 3), ("RET", 2)))
  }

  "Implicit return in if-else block" in {
    val cpg = code("""
                     |def foo(arg)
                     |if arg > 1
                     |        arg + 1
                     |else
                     |        arg + 10
                     |end
                     |end
                     |
                     |x = 1
                     |y = foo x
                     |puts y
                     |""".stripMargin)

    val src   = cpg.method.name("foo").parameter.index(1).l
    val sink  = cpg.call.name("puts").argument(1).l
    val flows = sink.reachableByFlows(src)
    flows.map(flowToResultPairs).toSet shouldBe
      Set(
        List(
          ("foo(self, arg)", 2),
          ("arg > 1", 3),
          ("arg + 1", 4),
          ("RET", 2),
          ("self.foo = def foo (...)", 2),
          ("self.foo = def foo (...)", -1),
          ("foo x", 11),
          ("foo(self, arg)", 2),
          ("RET", 2),
          ("foo x", 11),
          ("puts y", 12)
        )
      )
  }

  "Implicit return in if-else block and underlying function call" in {
    val cpg = code("""
                     |def add(arg)
                     |arg + 100
                     |end
                     |
                     |def foo(arg)
                     |if arg > 1
                     |        add(arg)
                     |else
                     |        add(arg)
                     |end
                     |end
                     |
                     |x = 1
                     |y = foo x
                     |puts y
                     |
                     |""".stripMargin)

    val src   = cpg.method.name("foo").parameter.index(1).l
    val sink  = cpg.call.name("puts").argument(1).l
    val flows = sink.reachableByFlows(src)
    flows.map(flowToResultPairs).toSet shouldBe
      Set(
        List(
          ("foo(self, arg)", 6),
          ("arg > 1", 7),
          ("add(arg)", 8),
          ("add(self, arg)", 2),
          ("arg + 100", 3),
          ("RET", 2),
          ("add(arg)", 8),
          ("RET", 6),
          ("self.foo = def foo (...)", 6),
          ("self.foo = def foo (...)", -1),
          ("foo x", 15),
          ("foo(self, arg)", 6),
          ("RET", 6),
          ("foo x", 15),
          ("puts y", 16)
        )
      )
  }

  "Return via call w/o initialization" in {
    val cpg = code("""
                     |def add(p)
                     |  q = p
                     |  return q
                     |end
                     |
                     |n = 1
                     |ret = add(n)
                     |puts ret
                     |""".stripMargin)

    val src   = cpg.method.name("add").parameter.index(1).l
    val sink  = cpg.call.name("puts").argument(1).l
    val flows = sink.reachableByFlows(src)
    flows.map(flowToResultPairs).toSet shouldBe
      Set(
        List(
          ("add(self, p)", 2),
          ("q = p", 3),
          ("return q", 4),
          ("RET", 2),
          ("self.add = def add (...)", 2),
          ("self.add = def add (...)", -1),
          ("add(n)", 8),
          ("add(self, p)", 2),
          ("RET", 2),
          ("add(n)", 8),
          ("puts ret", 9)
        )
      )
  }
}
