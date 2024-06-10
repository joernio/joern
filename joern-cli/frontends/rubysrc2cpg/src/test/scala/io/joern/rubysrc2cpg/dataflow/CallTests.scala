package io.joern.rubysrc2cpg.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class CallTests extends RubyCode2CpgFixture(withPostProcessing = true, withDataFlow = true) {

  "Flow through call" in {
    val cpg = code("""
        |def foo(content)
        | puts content
        |end
        |
        |def main
        | n = 1
        | foo( n )
        |end
        |""".stripMargin)

    val src  = cpg.method.name("foo").parameter.where(_.index(1)).argument.l
    val sink = cpg.method.name("puts").callIn.argument(1).l
    sink.reachableByFlows(src).size shouldBe 1
  }

  "Explicit return via call with initialization" in {
    val cpg = code("""
                     |def add(p)
                     |  q = 5
                     |  q = p
                     |  return q
                     |end
                     |
                     |n = 1
                     |ret = add(n)
                     |puts ret
                     |""".stripMargin)

    val src  = cpg.literal.code("1").l
    val sink = cpg.call.name("puts").argument(1).l
    sink.reachableByFlows(src).size shouldBe 1
  }

  "Implicit return via call with initialization" in {
    val cpg = code("""
                     |def add(p)
                     |  q = 5
                     |  q = p
                     |  q
                     |end
                     |
                     |n = 1
                     |ret = add(n)
                     |puts ret
                     |""".stripMargin)

    val src  = cpg.literal.code("1").l
    val sink = cpg.call.name("puts").argument(1).l
    sink.reachableByFlows(src).size shouldBe 1
  }

  "Data flow through grouping expression with negation" in {
    val cpg = code("""
                     |def foo(arg)
                     |  return arg
                     |end
                     |
                     |x = false
                     |y = !(foo x)
                     |puts y
                     |
                     |""".stripMargin)

    val source = cpg.literal.code("false").l
    val sink   = cpg.call.name("puts").argument(1).l
    sink.reachableByFlows(source).size shouldBe 1
  }

  "Data flow through variable params" ignore {
    val cpg = code("""
                     |def foo(*args)
                     |  return args
                     |end
                     |
                     |x = 1
                     |y = foo("another param", x)
                     |puts y
                     |""".stripMargin)

    val source = cpg.literal.code("1").l
    val sink   = cpg.call.name("puts").argument(1).l
    sink.reachableByFlows(source).size shouldBe 1
  }

  "Data flow through optional params" in {
    val cpg = code("""
                     |def foo(arg=10)
                     |  return arg + 10
                     |end
                     |
                     |x = 1
                     |y = foo(x)
                     |puts y
                     |""".stripMargin)

    val source = cpg.literal.code("1").l
    val sink   = cpg.call.name("puts").argument(1).l
    sink.reachableByFlows(source).size shouldBe 1
  }

  "Data flow across files" in {
    val cpg = code(
      """
        |def my_func(x)
        | puts x
        |end
        |""".stripMargin,
      "foo.rb"
    )
      .moreCode(
        """
          |require_relative 'foo.rb'
          |x = 1
          |my_func(x)
          |""".stripMargin,
        "bar.rb"
      )

    val source = cpg.literal.code("1").l
    val sink   = cpg.call.name("puts").argument(1).l
    sink.reachableByFlows(source).size shouldBe 1
  }

  "Across the file data flow test" should {
    val cpg = code(
      """
        |def foo(arg)
        | puts arg
        | loop do
        | arg += 1
        |  if arg > 3
        |        puts arg
        |        return
        |  end
        | end
        |end
        |""".stripMargin,
      "foo.rb"
    )
      .moreCode(
        """
          |require_relative 'foo.rb'
          |x = 1
          |foo x
          |""".stripMargin,
        "bar.rb"
      )

    "be found for sink in outer block" in {
      val source = cpg.literal.code("1").l
      val sink   = cpg.call.name("puts").argument(1).lineNumber(3).l
      sink.reachableByFlows(source).size shouldBe 1
      val src = cpg.identifier("x").lineNumber(3).l
      sink.reachableByFlows(src).size shouldBe 1
    }

    "be found for sink in nested block" in {
      val src  = cpg.identifier("x").lineNumber(3).l
      val sink = cpg.call.name("puts").argument(1).lineNumber(7).l
      sink.reachableByFlows(src).size shouldBe 1
    }
  }

  // TODO: Generates warnings
  "Data flow through invocation or command with EMARK" in {
    val cpg = code("""
                     |x=12
                     |def woo(x)
                     |    return x == 10
                     |end
                     |
                     |if !woo x
                     |    puts x
                     |else
                     |    puts "No"
                     |end
                     |""".stripMargin)

    val source = cpg.literal.code("12").l
    val sink   = cpg.call.name("puts").argument(1).l
    sink.reachableByFlows(source).size shouldBe 1
  }

  "Flow for nested puts calls" in {
    val cpg = code("""
                     |x=10
                     |def put_name(x)
                     |    puts x
                     |end
                     |def nested_put(x)
                     |    put_name(x)
                     |end
                     |def double_nested_put(x)
                     |    nested_put(x)
                     |end
                     |double_nested_put(x)
                     |""".stripMargin)

    val source = cpg.literal.code("10").l
    val sink   = cpg.call.name("puts").argument(1).l
    sink.reachableByFlows(source).size shouldBe 1
  }

  "Data flow through a keyword? named method usage" in {
    val cpg = code("""
                     |x = 1
                     |y = x.nil?
                     |puts y
                     |""".stripMargin)

    val src  = cpg.literal.code("1").l
    val sink = cpg.call.name("puts").argument(1).l
    sink.reachableByFlows(src).size shouldBe 1
  }

  "Data flow through a keyword inside a association" in {
    val cpg = code("""
                     |def foo(arg)
                     |  puts arg
                     |end
                     |
                     |x = 1
                     |foo if: x.nil?
                     |""".stripMargin)

    val src  = cpg.literal.code("1").l
    val sink = cpg.call.name("puts").argument(1).l
    sink.reachableByFlows(src).size shouldBe 1
  }

  "flow through a method call with safe navigation operator with parentheses" in {
    val cpg = code("""
                     |class Foo
                     | def bar(arg)
                     |   return arg
                     | end
                     |end
                     |x=1
                     |foo = Foo.new
                     |y = foo&.bar(x)
                     |puts y
                     |""".stripMargin)

    val src  = cpg.literal.code("1").l
    val sink = cpg.call.name("puts").argument(1).l
    src.reachableByFlows(src).size shouldBe 1
  }

  // TODO: This does not create a call node, as AMPDOT is not recognized by the parser without parentheses
  "flow through a method call with safe navigation operator without parentheses" ignore {
    val cpg = code("""
                     |class Foo
                     | def bar(arg)
                     |   return arg
                     | end
                     |end
                     |x=1
                     |foo = Foo.new
                     |y = foo&.bar x
                     |puts y
                     |""".stripMargin)

    val src  = cpg.literal.code("1").l
    val sink = cpg.call.name("puts").argument(1).l
    sink.reachableByFlows(src).size shouldBe 1
  }

  "flow through a method call present in next line, with the second line starting with `.`" in {
    val cpg = code("""
                     |class Foo
                     | def bar(x)
                     |   return x
                     | end
                     |end
                     |
                     |x = 1
                     |foo = Foo.new
                     |y = foo
                     | .bar(1)
                     |puts y
                     |""".stripMargin)

    val src        = cpg.literal.code("1").l
    val sink       = cpg.call.name("puts").argument(1).l
    val List(flow) = sink.reachableByFlows(src).map(flowToResultPairs).distinct.sortBy(_.length).l
    flow shouldBe List(
      (
        """|foo
           | .bar(1)""".stripMargin,
        11
      ),
      ("bar(self, x)", 3),
      ("return x", 4),
      ("RET", 3),
      (
        """|foo
           | .bar(1)""".stripMargin,
        10
      ),
      (
        """|y = foo
           | .bar(1)""".stripMargin,
        10
      ),
      ("puts y", 12)
    )
  }

  "flow through a method call present in next line, with the first line ending with `.`" in {
    val cpg = code("""
                     |class Foo
                     | def bar(x)
                     |   return x
                     | end
                     |end
                     |
                     |x = 1
                     |foo = Foo.new
                     |y = foo.
                     |  bar(1)
                     |puts y
                     |""".stripMargin)

    val src        = cpg.literal.code("1").l
    val sink       = cpg.call.name("puts").argument(1).l
    val List(flow) = sink.reachableByFlows(src).map(flowToResultPairs).distinct.sortBy(_.length).l
    flow shouldBe List(
      (
        """|foo.
           |  bar(1)""".stripMargin,
        11
      ),
      ("bar(self, x)", 3),
      ("return x", 4),
      ("RET", 3),
      (
        """|foo.
           |  bar(1)""".stripMargin,
        10
      ),
      (
        """|y = foo.
           |  bar(1)""".stripMargin,
        10
      ),
      ("puts y", 12)
    )
  }
}
