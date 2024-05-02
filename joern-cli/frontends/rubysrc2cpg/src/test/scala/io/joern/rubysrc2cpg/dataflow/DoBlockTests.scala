package io.joern.rubysrc2cpg.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class DoBlockTests extends RubyCode2CpgFixture(withPostProcessing = true, withDataFlow = true) {
  // Works in deprecated
  "Data flow through chainedInvocationPrimary usage" ignore {
    val cpg = code("""
                     |x = 1
                     |
                     |[x, x+1].each do |number|
                     |  puts "#{number} was passed to the block"
                     |end
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).l.size shouldBe 2
  }

  // TODO:
  "Data flow coming out of chainedInvocationPrimary usage" ignore {
    val cpg = code("""
                     |x = 1
                     |y = 10
                     |[x, x+1].each do |number|
                     |  y += x
                     |end
                     |puts y
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).l.size shouldBe 2
  }

  // Works in deprecated
  "Data flow through chainedInvocationPrimary without arguments to block usage" ignore {
    val cpg = code("""
                     |x = 1
                     |
                     |[1,2,3].each do
                     |  puts "Right here #{x}"
                     |end
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).l.size shouldBe 1
  }

  // Works in deprecated
  "Data flow through argsAndDoBlockAndMethodIdCommandWithDoBlock" ignore {
    val cpg = code("""
                     |def foo (blockArg,&block)
                     |block.call(blockArg)
                     |end
                     |
                     |x = 10
                     |foo :a_symbol do |arg|
                     |  y = x + arg.length
                     |  puts y
                     |end
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  // Works in deprecated
  "Data flow through primaryMethodArgsDoBlockCommandWithDoBlock" ignore {
    val cpg = code("""
                     |module FooModule
                     |def foo (blockArg,&block)
                     |block.call(blockArg)
                     |end
                     |end
                     |
                     |x = 10
                     |FooModule.foo :a_symbol do |arg|
                     |  y = x + arg.length
                     |  puts y
                     |end
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  "Data flow through break with args" in {
    val cpg = code("""
                     |x = 1
                     |arr = [x, 2, 3]
                     |y = arr.each do |num|
                     |  break num if num < 2
                     |  puts num
                     |end
                     |puts y
                     |
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 4
  }

  // TODO:
  "Data flow through next with args" ignore {
    val cpg = code("""
                     |x = 10
                     |a = [1, 2, 3]
                     |y = a.map do |num|
                     |  next x if num.even?
                     |  num
                     |end
                     |
                     |puts y
                     |
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  // Works in deprecated
  "Data flow for chained command with do-block with parentheses" ignore {
    val cpg = code("""
                     |def foo()
                     |  yield if block_given?
                     |end
                     |
                     |y = foo do
                     |    x = 1
                     |    [x+1,x+2]
                     |end.sum(10)
                     |
                     |puts y
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  // Works in deprecated
  "Data flow for chained command with do-block without parentheses" ignore {
    val cpg = code("""
                     |def foo()
                     |  yield if block_given?
                     |end
                     |
                     |y = foo do
                     |    x = 1
                     |    [x+1,x+2]
                     |end.sum 10
                     |
                     |puts y
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  // Works in deprecated
  "Data flow for yield block specified alongwith the call" ignore {
    val cpg = code("""
                     |x=10
                     |def foo(x)
                     |    a = yield
                     |    puts a
                     |end
                     |
                     |foo(x) {
                     |    x + 2
                     |}
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 1
    /*
     * TODO the flow count shows 1 since the origin is considered as x + 2
     * The actual origin is x=10. However, this is not considered since there is
     * no REACHING_DEF edge from the x of 'x=10' to the x of 'x + 2'.
     * There are already other disabled data flow test cases for this problem. Once solved, it should
     * be possible to set the required count to 2
     */

  }
}
