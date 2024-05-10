package io.joern.rubysrc2cpg.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class ArrayTests extends RubyCode2CpgFixture(withPostProcessing = true, withDataFlow = true) {

  "Data flow through array constructor expressionsOnlyIndexingArguments" in {
    val cpg = code("""
                     |x = 1
                     |array = [x,2]
                     |puts x
                     |""".stripMargin)

    val source = cpg.literal.code("1").l
    val sink   = cpg.call.name("puts").argument(1).l
    sink.reachableByFlows(source).size shouldBe 1
  }

  "Data flow through array constructor splattingOnlyIndexingArguments" in {
    val cpg = code("""
                     |def foo(*splat_args)
                     |  array = [*splat_args]
                     |  puts array
                     |end
                     |
                     |x = 1
                     |y = 2
                     |y = foo(x,y)
                     |""".stripMargin)

    val source = cpg.literal.code("1").l
    val sink   = cpg.call.name("puts").argument(1).l
    sink.reachableByFlows(source).size shouldBe 1
  }

  "Data flow through array constructor expressionsAndSplattingIndexingArguments" in {
    val cpg = code("""
                     |def foo(*splat_args)
                     |  array = [1,2,*splat_args]
                     |  puts array
                     |end
                     |
                     |x = 3
                     |foo(x)
                     |""".stripMargin)

    val source = cpg.literal.code("3").l
    val sink   = cpg.call.name("puts").argument(1).l
    sink.reachableByFlows(source).size shouldBe 1
  }

  "Data flow through array constructor associationsOnlyIndexingArguments" in {
    val cpg = code("""
                     |def foo(arg)
                     |  array = [1 => arg, 2 => arg]
                     |  puts array
                     |end
                     |
                     |x = 3
                     |foo(x)
                     |""".stripMargin)

    val source = cpg.literal.code("3").l
    val sink   = cpg.call.name("puts").argument(1).l
    sink.reachableByFlows(source).size shouldBe 1
  }

  "Data flow through array constructor commandOnlyIndexingArguments" in {
    val cpg = code("""
                     |def increment(arg)
                     |  return arg + 1
                     |end
                     |
                     |x = 1
                     |array = [increment(x), increment(x+1)]
                     |puts array
                     |
                     |""".stripMargin)

    val source = cpg.literal.code("1").lineNumber(6).l
    val sink   = cpg.call.name("puts").argument(1).l
    sink.reachableByFlows(source).size shouldBe 1
  }

  "Data flow through indexingExpressionPrimary" in {
    val cpg = code("""
                     |x = [1,2,3]
                     |y = x[0]
                     |puts y
                     |""".stripMargin)

    val source = cpg.identifier.name("x").lineNumber(2).l
    val sink   = cpg.call.name("puts").argument(1).l
    sink.reachableByFlows(source).size shouldBe 1
  }

  // Works in deprecated
  "Data flow through array assignments" ignore {
    val cpg = code("""
                     |x = 10
                     |array = [0, 1]
                     |array[0] = x
                     |puts array
                     |
                     |""".stripMargin)

    val source = cpg.literal.code("10").l
    val sink   = cpg.call.name("puts").argument(1).l
    sink.reachableByFlows(source).size shouldBe 1
  }

  "flow through %w array" in {
    val cpg = code("""
                     |a = %w[b c]
                     |puts a
                     |""".stripMargin)

    val source = cpg.literal.code("b").l
    val sink   = cpg.call.name("puts").argument(1).l
    sink.reachableByFlows(source).size shouldBe 2
  }

  "flow through %i array" in {
    val cpg = code("""
                     |a = %i[b
                     |    c]
                     |puts a
                     |""".stripMargin)

    val source = cpg.literal.code("b").l
    val sink   = cpg.call.name("puts").argument(1).l
    sink.reachableByFlows(source).size shouldBe 2
  }

  "flow through array constructor using []" in {
    val cpg = code("""
                     |x=1
                     |y=x
                     |z = Array[y,2]
                     |puts "#{z}"
                     |""".stripMargin)

    val source = cpg.literal.code("1").l
    val sink   = cpg.call.name("puts").argument(1).l
    sink.reachableByFlows(source).size shouldBe 1
  }

  "flow through array constructor using [] and command in []" in {
    val cpg = code("""
                     |def foo(arg)
                     |  return arg
                     |end
                     |
                     |x=1
                     |y=x
                     |z = Array[foo y]
                     |puts "#{z}"
                     |""".stripMargin)

    val source = cpg.literal.code("1").l
    val sink   = cpg.call.name("puts").argument(1).l
    sink.reachableByFlows(source).size shouldBe 1
  }

}
