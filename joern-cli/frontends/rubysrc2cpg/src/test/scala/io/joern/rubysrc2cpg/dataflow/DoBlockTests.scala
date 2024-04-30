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

  "Data flow through primaryMethodArgsDoBlockCommandWithDoBlock" in {
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

}
