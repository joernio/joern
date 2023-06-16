package io.joern.rubysrc2cpg.dataflow

import io.joern.rubysrc2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.joern.dataflowengineoss.language._
import io.shiftleft.semanticcpg.language._

class DataFlowTests extends DataFlowCodeToCpgSuite {

  "Data flow through if-elseif-else" should {
    val cpg = code("""
        |x = 2
        |a = x
        |b = 0
        |
        |if a > 2
        |    b = a + 3
        |elseif a > 4
        |    b = a + 5
        |elseif a > 8
        |    b = a + 5
        |else
        |    b = a + 9
        |end
        |
        |puts(b)
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).l.size shouldBe 2
    }
  }

  "Flow via call" should {
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

    "be found" in {
      implicit val resolver: ICallResolver = NoResolve
      val src                              = cpg.identifier.name("n").where(_.inCall.name("print")).l
      val sink                             = cpg.method.name("puts").callIn.argument(1).l
      sink.reachableByFlows(src).size shouldBe 1
    }
  }

  "Explicit return via call with initialization" should {
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

    "be found" in {
      val src  = cpg.identifier.name("n").l
      val sink = cpg.call.name("puts").l
      sink.reachableByFlows(src).l.size shouldBe 2
    }
  }

  "Implicit return via call with initialization" should {
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

    "be found" in {
      val src  = cpg.identifier.name("n").l
      val sink = cpg.call.name("puts").l
      sink.reachableByFlows(src).l.size shouldBe 2
    }
  }

  "Return via call w/o initialization" should {
    val cpg = code("""
        |def add(p)
        |q = p
        |return q
        |end
        |
        |n = 1
        |ret = add(n)
        |puts ret
        |""".stripMargin)

    "be found" in {
      val src  = cpg.identifier.name("n").l
      val sink = cpg.call.name("puts").l
      sink.reachableByFlows(src).l.size shouldBe 2
    }
  }

  "Data flow in a while loop" should {
    val cpg = code("""
        |i = 0
        |num = 5
        |
        |while i < num  do
        |   num = i + 3
        |end
        |puts num
        |""".stripMargin)

    "be found" in {
      val src  = cpg.identifier.name("i").l
      val sink = cpg.call.name("puts").l
      sink.reachableByFlows(src).l.size shouldBe 3
    }
  }

  "Data flow in a while modifier" should {
    val cpg = code("""
        |i = 0
        |num = 5
        |begin
        |   num = i + 3
        |end while i < num
        |puts num
        |""".stripMargin)

    "be found" in {
      val src  = cpg.identifier.name("i").l
      val sink = cpg.call.name("puts").l
      sink.reachableByFlows(src).l.size shouldBe 3
    }
  }

  "Data flow through expressions" should {
    val cpg = code("""
        |a = 1
        |b = a+3
        |c = 2 + b%6
        |d = c + b & !c + -b
        |e = c/d + b || d
        |f = c - d & ~e
        |g = f-c%d - +d
        |h = g**5 << b*g
        |i = b && c || e > g
        |j = b>c ? (e+-6) : (f +5)
        |k = i..h
        |l = j...g
        |
        |puts l
        |""".stripMargin)

    "be found" in {
      val src  = cpg.identifier.name("a").l
      val sink = cpg.call.name("puts").l
      sink.reachableByFlows(src).l.size shouldBe 2
    }
  }

  "Data flow through multiple assignments" ignore {
    // TODO test a lot more multiple assignments
    val cpg = code("""
        |a = 1
        |b = 2
        |c,d=a,b
        |puts c
        |""".stripMargin)

    "be found" in {
      val src  = cpg.identifier.name("a").l
      val sink = cpg.call.name("puts").l
      sink.reachableByFlows(src).l.size shouldBe 2
    }
  }

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

    "be found" in {
      val src  = cpg.identifier.name("a").l
      val sink = cpg.call.name("puts").l
      sink.reachableByFlows(src).l.size shouldBe 2
    }
  }

  "Data flow through module method" ignore {
    val cpg = code("""
        |module MyModule
        |  def MyModule.print(text)
        |    puts text
        |  end
        |end
        |
        |x = "some text"
        |
        |MyModule::print(x)
        |""".stripMargin)

    "be found" in {
      val src  = cpg.identifier.name("a").l
      val sink = cpg.call.name("puts").l
      sink.reachableByFlows(src).l.size shouldBe 2
    }
  }

  "Data flow through yield with argument" ignore {
    val cpg = code("""
        |def yield_with_arguments
        |  a = "something"
        |  yield(a)
        |end
        |
        |yield_with_arguments { |arg| puts "Argument is #{arg}" }
        |""".stripMargin)

    "be found" in {
      val src  = cpg.identifier.name("a").l
      val sink = cpg.call.name("puts").l
      sink.reachableByFlows(src).l.size shouldBe 2
    }
  }

  "Data flow through a until loop" should {
    val cpg = code("""
        |i = 0
        |num = 5
        |
        |until i < num
        |   num = i + 3
        |end
        |puts num
        |""".stripMargin)

    "be found" in {
      val src  = cpg.identifier.name("i").l
      val sink = cpg.call.name("puts").l
      sink.reachableByFlows(src).l.size shouldBe 3
    }
  }

  "Data flow in through until modifier" should {
    val cpg = code("""
        |i = 0
        |num = 5
        |begin
        |   num = i + 3
        |end until i < num
        |puts num
        |""".stripMargin)

    "be found" in {
      val src  = cpg.identifier.name("i").l
      val sink = cpg.call.name("puts").l
      sink.reachableByFlows(src).l.size shouldBe 3
    }
  }

  "Data flow through unless-else" should {
    val cpg = code("""
        |x = 2
        |a = x
        |b = 0
        |
        |unless a > 2
        |    b = a + 3
        |else
        |    b = a + 9
        |end
        |
        |puts(b)
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).l.size shouldBe 2
    }
  }

  "Data flow through case statement" ignore {
    val cpg = code("""
        |x = 2
        |b = x
        |
        |case b
        |when 1
        |    puts b
        |when 2
        |    puts b
        |when 3
        |    puts b
        |else
        |    puts b
        |end
        |
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).l.size shouldBe 2
    }
  }

  "Data flow through do-while loop" should {
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

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).l.size shouldBe 2
    }
  }

  "Data flow through for loop" should {
    val cpg = code("""
          |x = 0
          |arr = [1,2,3,4,5]
          |num = 0
          |for i in arr do
          |   y = x + i
          |   num = y*i
          |end
          |puts num
          |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).l.size shouldBe 2
    }
  }

  "Data flow through for loop simple" should {
    val cpg = code("""
        |x = 0
        |arr = [1,2,3,4,5]
        |num = 0
        |for i in arr do
        |   num = x
        |end
        |puts num
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).l.size shouldBe 2
    }
  }

  "Data flow through for and next AFTER statement" should {
    val cpg = code("""
        |x = 0
        |arr = [1,2,3,4,5]
        |num = 0
        |for i in arr do
        |   num = x
        |   next if i % 2 == 0
        |end
        |puts num
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).l.size shouldBe 2
    }
  }

  "Data flow through for and next BEFORE statement" should {
    val cpg = code("""
        |x = 0
        |arr = [1,2,3,4,5]
        |num = 0
        |for i in arr do
        |   next if i % 2 == 0
        |   num = x
        |end
        |puts num
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).l.size shouldBe 2
    }
  }

  "Data flow through for and redo AFTER statement" should {
    val cpg = code("""
        |x = 0
        |arr = [1,2,3,4,5]
        |num = 0
        |for i in arr do
        |   num = x
        |   redo if i % 2 == 0
        |end
        |puts num
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).l.size shouldBe 2
    }
  }

  "Data flow through for and redo BEFORE statement" should {
    val cpg = code("""
        |x = 0
        |arr = [1,2,3,4,5]
        |num = 0
        |for i in arr do
        |   redo if i % 2 == 0
        |   num = x
        |end
        |puts num
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).l.size shouldBe 2
    }
  }

  "Data flow through for and retry AFTER statement" should {
    val cpg = code("""
        |x = 0
        |arr = [1,2,3,4,5]
        |num = 0
        |for i in arr do
        |   num = x
        |   retry if i % 2 == 0
        |end
        |puts num
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).l.size shouldBe 2
    }
  }

  "Data flow through for and retry BEFORE statement" should {
    val cpg = code("""
        |x = 0
        |arr = [1,2,3,4,5]
        |num = 0
        |for i in arr do
        |   retry if i % 2 == 0
        |   num = x
        |end
        |puts num
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).l.size shouldBe 2
    }
  }

  "Data flow through grouping expression" should {
    val cpg = code("""
        |x = 0
        |y = (x==0)
        |puts y
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).l.size shouldBe 2
    }
  }

  "Data flow through variable assigned a scoped constant" should {
    val cpg = code("""
        |MyConst = 10
        |x = ::MyConst
        |puts x
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).l.size shouldBe 2
    }
  }

  "Data flow through variable assigned a chained scoped constant" should {
    val cpg = code("""
        |MyConst = 10
        |x = ::MyConst
        |puts x
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).l.size shouldBe 2
    }
  }

  "Data flow through array constructor expressionsOnlyIndexingArguments" should {
    val cpg = code("""
        |x = 1
        |array = [x,2]
        |puts x
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).l.size shouldBe 3
    }
  }

  "Data flow through array constructor splattingOnlyIndexingArguments" should {
    val cpg = code("""
        |def foo(*splat_args)
        |array = [*splat_args]
        |puts array
        |end
        |
        |x = 1
        |y = 2
        |y = foo(x,y)
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).l.size shouldBe 2
    }
  }

  "Data flow through array constructor expressionsAndSplattingIndexingArguments" should {
    val cpg = code("""
        |def foo(*splat_args)
        |array = [1,2,*splat_args]
        |puts array
        |end
        |
        |x = 3
        |foo(x)
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).l.size shouldBe 2
    }
  }

  "Data flow through array constructor associationsOnlyIndexingArguments" should {
    val cpg = code("""
        |def foo(arg)
        |array = [1 => arg, 2 => arg]
        |puts array
        |end
        |
        |x = 3
        |foo(x)
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).l.size shouldBe 2
    }
  }

  "Data flow through array constructor commandOnlyIndexingArguments" should {
    val cpg = code("""
        |def increment(arg)
        |return arg + 1
        |end
        |
        |x = 1
        |array = [ increment(x), increment(x+1)]
        |puts array
        |
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).l.size shouldBe 3
    }
  }

  "Data flow through hash constructor" should {
    val cpg = code("""
        |def foo(arg)
        |hash = {1 => arg, 2 => arg}
        |puts hash
        |end
        |
        |x = 3
        |foo(x)
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).l.size shouldBe 2
    }
  }

  "Data flow through string interpolation" should {
    val cpg = code("""
        |x = 1
        |str = "The source is #{x}"
        |puts str
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).l.size shouldBe 2
    }
  }

  "Data flow through indexingExpressionPrimary" should {
    val cpg = code("""
        |x = [1,2,3]
        |y = x[0]
        |puts y
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).l.size shouldBe 2
    }
  }

  "Data flow through methodOnlyIdentifier usage" should {
    val cpg = code("""
        |x = 1
        |y = SomeConstant! + x
        |puts y
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).l.size shouldBe 2
    }
  }

  "Data flow through chainedInvocationPrimary usage" should {
    val cpg = code("""
        |x = 1
        |
        |[x, x+1].each do |number|
        |  puts "#{number} was passed to the block"
        |end
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).l.size shouldBe 3
    }
  }

  "Data flow through chainedInvocationWithoutArgumentsPrimary usage" should {
    val cpg = code("""
        |x = 1
        |
        |[1,2,3].each do
        |  puts "Right here #{x}"
        |end
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).l.size shouldBe 1
    }
  }

  "Data flow through invocationWithBlockOnlyPrimary usage" should {
    val cpg = code("""
        |def hello(&block)
        |  block.call
        |end
        |
        |x = "hello"
        |hello { puts x }
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).l.size shouldBe 2
    }
  }
}
