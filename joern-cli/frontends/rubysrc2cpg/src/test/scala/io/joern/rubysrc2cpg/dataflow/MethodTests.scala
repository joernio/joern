package io.joern.rubysrc2cpg.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class MethodTests extends RubyCode2CpgFixture(withPostProcessing = true, withDataFlow = true) {
  // Works in deprecated
  "Data flow through class method" ignore {
    val cpg = code("""
                     |class MyClass
                     |  def print(text)
                     |    puts text
                     |  end
                     |end
                     |
                     |
                     |x = "some text"
                     |inst = MyClass.new
                     |inst.print(x)
                     |""".stripMargin)

    val src  = cpg.identifier.name("x").l
    val sink = cpg.call.name("puts").l
    sink.reachableByFlows(src).l.size shouldBe 2
  }

  // Works in deprecated
  "Data flow through do-while loop" ignore {
    val cpg = code("""
                     |x = 0
                     |num = -1
                     |loop do
                     |   num = x + 1
                     |   x = x + 1
                     |   if x > 10
                     |     break
                     |   end
                     |end
                     |puts num
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).l.size shouldBe 2
  }

  "Data flow through methodOnlyIdentifier usage" in {
    val cpg = code("""
                     |x = 1
                     |y = SomeConstant! + x
                     |puts y
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).l.size shouldBe 2
  }

  // Works in deprecated
  "Data flow through blockExprAssocTypeArguments" ignore {
    val cpg = code("""
                     |def foo(*args)
                     |puts args
                     |end
                     |
                     |x = "value1"
                     |foo(key1: x, key2: "value2")
                     |
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  // Works in deprecated - Unsupported element type SplattingArgumentArgumentList
  "Data flow through blockSplattingTypeArguments" ignore {
    val cpg = code("""
                     |def foo(arg)
                     |puts arg
                     |end
                     |
                     |x = 1
                     |foo(*x)
                     |
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  // Works in deprecated
  "Data flow through blockSplattingExprAssocTypeArguments without block" ignore {
    val cpg = code("""
                     |def foo(*arg)
                     |puts arg
                     |end
                     |
                     |x = 1
                     |foo( x+1, key1: x*2, key2: x*3 )
                     |
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  // Works in deprecated - Unsupported element type SplattingArgumentArgumentList
  "Data flow through blockSplattingTypeArguments without block" ignore {
    val cpg = code("""
                     |def foo (blockArg,&block)
                     |block.call(blockArg)
                     |end
                     |
                     |x = 10
                     |foo(*x do |arg|
                     |  y = x + arg
                     |  puts y
                     |end
                     |)
                     |
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  "Data flow through blockExprAssocTypeArguments with block argument in the wrapper function" in {
    val cpg = code("""
                     |def foo (blockArg,&block)
                     |block.call(blockArg)
                     |end
                     |
                     |def foo_wrap (blockArg,&block)
                     |foo(blockArg,&block)
                     |end
                     |
                     |
                     |x = 10
                     |foo_wrap x do |arg|
                     |  y = 100 + arg
                     |  puts y
                     |end
                     |
                     |
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }
}
