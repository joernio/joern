package io.joern.rubysrc2cpg.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class CallTests extends RubyCode2CpgFixture(withPostProcessing = true, withDataFlow = true) {

  // Works on deprecated
  "Flow through call" ignore {
    val cpg = code("""
        |def print(content)
        |puts content
        |end
        |
        |def main
        |n = 1
        |print( n )
        |end
        |""".stripMargin)

    val src  = cpg.method.name("print").parameter.where(_.index(1)).argument.l
    val sink = cpg.method.name("puts").callIn.argument(1).l
    sink.reachableByFlows(src).size shouldBe 1
  }

  // Works on deprecated
  "Explicit return via call with initialization" ignore {
    val cpg = code("""
                     |def add(p)
                     |q = 5
                     |q = p
                     |return q
                     |end
                     |
                     |n = 1
                     |ret = add(n)
                     |puts ret
                     |""".stripMargin)

    val src  = cpg.identifier.name("n").l
    val sink = cpg.call.name("puts").l
    sink.reachableByFlows(src).l.size shouldBe 2
  }

  // Works on deprecated
  "Implicit return via call with initialization" ignore {
    val cpg = code("""
                     |def add(p)
                     |q = 5
                     |q = p
                     |q
                     |end
                     |
                     |n = 1
                     |ret = add(n)
                     |puts ret
                     |""".stripMargin)

    val src  = cpg.identifier.name("n").l
    val sink = cpg.call.name("puts").l
    sink.reachableByFlows(src).l.size shouldBe 2
  }

  // Works in deprecated
  "Data flow through grouping expression with negation" ignore {
    val cpg = code("""
                     |def foo(arg)
                     |return arg
                     |end
                     |
                     |x = false
                     |y = !(foo x)
                     |puts y
                     |
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  // Works in deprecated
  "Data flow through variable params" ignore {
    val cpg = code("""
                     |def foo(*args)
                     |  return args
                     |end
                     |
                     |x = 1
                     |y = foo(x, "another param")
                     |puts y
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  // Works in deprecated
  "Data flow through optional params" ignore {
    val cpg = code("""
                     |def foo(arg=10)
                     |  return arg + 10
                     |end
                     |
                     |x = 1
                     |y = foo(x)
                     |puts y
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  // Works in deprecated
  "Data flow across files" ignore {
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

  // Works in deprecated
  "Across the file data flow test" ignore {
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

    val source = cpg.literal.code("1").l
    val sink   = cpg.call.name("puts").argument(1).lineNumber(3).l
    sink.reachableByFlows(source).size shouldBe 1
    val src = cpg.identifier("x").lineNumber(3).l
    sink.reachableByFlows(src).size shouldBe 1

//    // TODO: Need to be fixed.
//    "be found for sink in nested block" ignore {
//      val src  = cpg.identifier("x").lineNumber(3).l
//      val sink = cpg.call.name("puts").argument(1).lineNumber(7).l
//      sink.reachableByFlows(src).size shouldBe 1
//    }
  }

  // Works in deprecated - does not parse on new frontend
  "Data flow through invocation or command with EMARK" ignore {
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

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 3
  }

  // Works in deprecated
  "Flow for nested puts calls" ignore {
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

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 5
  }

  "Data flow through a keyword? named method usage" in {
    val cpg = code("""
                     |x = 1
                     |y = x.nil?
                     |puts y
                     |""".stripMargin)

    val src  = cpg.identifier.name("x").l
    val sink = cpg.call.name("puts").l
    sink.reachableByFlows(src).size shouldBe 2
  }

  // Works in deprecated
  "Data flow through a keyword inside a association" ignore {
    val cpg = code("""
                     |def foo(arg)
                     |puts arg
                     |end
                     |
                     |x = 1
                     |foo if: x.nil?
                     |""".stripMargin)

    val src  = cpg.identifier.name("x").l
    val sink = cpg.call.name("puts").l
    sink.reachableByFlows(src).size shouldBe 2
  }

  // Works in deprecated
  "flow through a method call with safe navigation operator with parantheses" ignore {
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

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  // Works in deprecated
  "flow through a method call with safe navigation operator without parantheses" ignore {
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

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
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

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 1
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

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 1
  }
}
