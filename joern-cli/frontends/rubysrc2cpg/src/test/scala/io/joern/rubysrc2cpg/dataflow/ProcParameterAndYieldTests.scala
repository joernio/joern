package io.joern.rubysrc2cpg.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class ProcParameterAndYieldTests extends RubyCode2CpgFixture(withPostProcessing = true, withDataFlow = true) {
  // Works in deprecated
  "Data flow through yield with argument having parenthesis" ignore {
    val cpg = code("""
                     |def yield_with_arguments
                     |  a = "something"
                     |  yield(a)
                     |end
                     |
                     |yield_with_arguments { |arg| puts "Argument is #{arg}" }
                     |""".stripMargin)

    val src  = cpg.identifier.name("a").l
    val sink = cpg.call.name("puts").l
    sink.reachableByFlows(src).size shouldBe 2
  }

  // Works in deprecated
  "Data flow through yield with argument without parenthesis and multiple yield blocks" ignore {
    val cpg = code("""
                     |def yield_with_arguments
                     |  x = "something"
                     |  y = "something_else"
                     |  yield(x,y)
                     |end
                     |
                     |yield_with_arguments { |arg1, arg2| puts "Yield block 1 #{arg1} and #{arg2}" }
                     |yield_with_arguments { |arg1, arg2| puts "Yield block 2 #{arg2} and #{arg1}" }
                     |""".stripMargin)

    val src  = cpg.identifier.name("x").l
    val sink = cpg.call.name("puts").l
    sink.reachableByFlows(src).size shouldBe 4
  }

  // Works in deprecated
  "Data flow through yield without argument" ignore {
    val cpg = code("""
                     |x = 1
                     |def yield_method
                     |  yield
                     |end
                     |yield_method { puts x }
                     |""".stripMargin)

    val src  = cpg.identifier.name("x").l
    val sink = cpg.call.name("puts").l
    sink.reachableByFlows(src).size shouldBe 1
  }

  // Works in deprecated
  "Data flow coming out of yield without argument" ignore {
    val cpg = code("""
                     |def foo
                     |        x=10
                     |        z = yield
                     |        puts z
                     |end
                     |
                     |x = 100
                     |foo{ x + 10 }
                     |""".stripMargin)

    val src  = cpg.identifier.name("x").l
    val sink = cpg.call.name("puts").l
    sink.reachableByFlows(src).size shouldBe 1
  }

  // TODO: From deprecated
  "Data flow through yield with argument and multiple yield blocks" ignore {
    val cpg = code("""
                     |def yield_with_arguments
                     |  x = "something"
                     |  y = "something_else"
                     |  yield(x)
                     |  yield(y)
                     |end
                     |
                     |yield_with_arguments { |arg| puts "Yield block 1 #{arg}" }
                     |yield_with_arguments { |arg| puts "Yield block 2 #{arg}" }
                     |""".stripMargin)

    val src1  = cpg.identifier.name("x").l
    val sink1 = cpg.call.name("puts").l
    sink1.reachableByFlows(src1).size shouldBe 2

    val src2  = cpg.identifier.name("y").l
    val sink2 = cpg.call.name("puts").l
    sink2.reachableByFlows(src2).size shouldBe 2
  }

  "Data flow through invocationWithBlockOnlyPrimary usage" in {
    val cpg = code("""
                     |def hello(&block)
                     |  block.call
                     |end
                     |
                     |x = "hello"
                     |hello { puts x }
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).l.size shouldBe 2
  }

  "Data flow through invocationWithBlockOnlyPrimary and method name starting with capital usage" in {
    val cpg = code("""
                     |def Hello(&block)
                     | block.call
                     |end
                     |x = "hello"
                     |Hello = "this should not be used"
                     |Hello { puts x }
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).l.size shouldBe 2
  }
}
