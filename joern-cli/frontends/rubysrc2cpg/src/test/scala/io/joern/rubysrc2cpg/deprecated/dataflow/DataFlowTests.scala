package io.joern.rubysrc2cpg.deprecated.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class DataFlowTests
    extends RubyCode2CpgFixture(withPostProcessing = true, withDataFlow = true, useDeprecatedFrontend = true) {

  "Data flow through if-elseif-else" should {
    val cpg = code("""
        |x = 2
        |a = x
        |b = 0
        |
        |if a > 2
        |    b = a + 3
        |elsif a > 4
        |    b = a + 5
        |elsif a > 8
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

  "Implicit return in if-else block" should {
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

    "be found" in {
      val src  = cpg.identifier.name("x").l
      val sink = cpg.call.name("puts").l
      sink.reachableByFlows(src).l.size shouldBe 2
    }
  }

  "Implicit return in if-else block and underlying function call" should {
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

    "be found" in {
      val src  = cpg.identifier.name("x").l
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

  "Data flow through multiple assignments" should {
    val cpg = code("""
        |x = 1
        |y = 2
        |c, d = x, y
        |puts c
        |""".stripMargin)

    "be found" in {
      val src  = cpg.identifier.name("x").l
      val sink = cpg.call.name("puts").l
      sink.reachableByFlows(src).l.size shouldBe 2
    }
  }

  "Data flow through multiple assignments with grouping" should {
    val cpg = code("""
        |x = 1
        |y = 2
        |(c, d) = x, y
        |puts c
        |""".stripMargin)

    "be found" in {
      val src  = cpg.identifier.name("x").l
      val sink = cpg.call.name("puts").l
      sink.reachableByFlows(src).l.size shouldBe 2
    }
  }

  "Data flow through multiple assignments with multi level grouping" ignore {
    val cpg = code("""
        |x = 1
        |y = 2
        |z = 3
        |a,(b,c) = z,y,x
        |puts a
        |""".stripMargin)

    "be found" in {
      val src  = cpg.identifier.name("x").l
      val sink = cpg.call.name("puts").l
      sink.reachableByFlows(src).l.size shouldBe 2
    }
  }
  "Data flow through multiple assignments with grouping and method in RHS" should {
    val cpg = code("""
        |def foo()
        |x = 1
        |return x
        |end
        |
        |b = 2
        |(c, d) = foo, b
        |puts c
        |
        |""".stripMargin)

    "be found" in {
      val src  = cpg.identifier.name("x").l
      val sink = cpg.call.name("puts").l
      sink.reachableByFlows(src).l.size shouldBe 2
    }
  }

  "Data flow through single LHS and splatting RHS" should {
    val cpg = code("""
        |x=1
        |y=*x
        |puts y
        |""".stripMargin)

    "be found" in {
      val src  = cpg.identifier.name("x").l
      val sink = cpg.call.name("puts").l
      sink.reachableByFlows(src).l.size shouldBe 2
    }
  }

  "Data flow through class method" should {
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
      val src  = cpg.identifier.name("x").l
      val sink = cpg.call.name("puts").l
      sink.reachableByFlows(src).l.size shouldBe 2
    }
  }

  "Data flow through class member" should {
    val cpg = code("""
        |class MyClass
        | @instanceVariable
        |
        | def initialize(value)
        |        @instanceVariable = value
        | end
        |
        | def getValue()
        |        @instanceVariable
        | end
        |end
        |
        |x = 12345
        |inst = MyClass.new(x)
        |y = inst.getValue
        |puts y
        |""".stripMargin)

    "be found" in {
      val src  = cpg.identifier.name("x").l
      val sink = cpg.call.name("puts").l
      sink.reachableByFlows(src).l.size shouldBe 2
    }
  }

  "Data flow through module method" should {
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
      val src  = cpg.identifier.name("x").l
      val sink = cpg.call.name("puts").l
      sink.reachableByFlows(src).l.size shouldBe 2
    }
  }

  "Data flow through yield with argument having parenthesis" should {
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
      sink.reachableByFlows(src).size shouldBe 2
    }
  }

  "Data flow through yield with argument without parenthesis and multiple yield blocks" should {
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

    "be found" in {
      val src  = cpg.identifier.name("x").l
      val sink = cpg.call.name("puts").l
      sink.reachableByFlows(src).size shouldBe 4
    }
  }

  "Data flow through yield without argument" should {
    val cpg = code("""
        |x = 1
        |def yield_method
        |  yield
        |end
        |yield_method { puts x }
        |""".stripMargin)

    "be found" in {
      val src  = cpg.identifier.name("x").l
      val sink = cpg.call.name("puts").l
      sink.reachableByFlows(src).size shouldBe 1
    }
  }

  "Data flow coming out of yield without argument" should {
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

    "be found" in {
      val src  = cpg.identifier.name("x").l
      val sink = cpg.call.name("puts").l
      sink.reachableByFlows(src).size shouldBe 1
    }
  }
  // TODO:
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

    "be found" in {
      val src1  = cpg.identifier.name("x").l
      val sink1 = cpg.call.name("puts").l
      sink1.reachableByFlows(src1).size shouldBe 2

      val src2  = cpg.identifier.name("y").l
      val sink2 = cpg.call.name("puts").l
      sink2.reachableByFlows(src2).size shouldBe 2
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

  "Data flow through case statement" should {
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
      sink.reachableByFlows(source).size shouldBe 8
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
      sink.reachableByFlows(source).l.size shouldBe 2
    }
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

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).l.size shouldBe 2
    }
  }

  "Data flow through chainedInvocationPrimary without arguments to block usage" should {
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

  "Data flow through invocationWithBlockOnlyPrimary and method name starting with capital usage" should {
    val cpg = code("""
        |def Hello(&block)
        | block.call
        |end
        |x = "hello"
        |Hello = "this should not be used"
        |Hello { puts x }
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).l.size shouldBe 2
    }
  }

  "Data flow for begin/rescue with sink in begin" should {
    val cpg = code("""
        |x = 1
        |begin
        |  puts x
        |rescue SomeException
        |  puts "SomeException occurred"
        |rescue => exceptionVar
        |  puts "Caught exception in variable #{exceptionVar}"
        |rescue
        |  puts "Catch-all block"
        |end
        |
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "Data flow for begin/rescue with sink in else" should {
    val cpg = code("""
        |x = 1
        |begin
        |  puts "In begin"
        |rescue SomeException
        |  puts "SomeException occurred"
        |rescue => exceptionVar
        |  puts "Caught exception in variable #{exceptionVar}"
        |rescue
        |  puts "Catch-all block"
        |else
        |  puts x
        |end
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "Data flow for begin/rescue with sink in rescue" should {
    val cpg = code("""
        |x = 1
        |begin
        |  puts "in begin"
        |rescue SomeException
        |  puts x
        |rescue => exceptionVar
        |  puts "Caught exception in variable #{exceptionVar}"
        |rescue
        |  puts "Catch-all block"
        |end
        |
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "Data flow for begin/rescue with sink in rescue with exception var" should {
    val cpg = code("""
        |begin
        |  puts "in begin"
        |rescue SomeException
        |  puts "SomeException occurred"
        |rescue => x
        |  y = x
        |  puts "Caught exception in variable #{y}"
        |rescue
        |  puts "Catch-all block"
        |end
        |
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 1
    }
  }

  "Data flow for begin/rescue with sink in catch-all rescue" should {
    val cpg = code("""
        |x = 1
        |begin
        |  puts "in begin"
        |rescue SomeException
        |   puts "SomeException occurred"
        |rescue => exceptionVar
        |  puts "Caught exception in variable #{exceptionVar}"
        |rescue
        |  puts x
        |end
        |
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "Data flow for begin/rescue with sink in ensure" should {
    val cpg = code("""
        |x = 1
        |begin
        |  puts "in begin"
        |rescue SomeException
        |   puts "SomeException occurred"
        |rescue => exceptionVar
        |  puts "Caught exception in variable #{exceptionVar}"
        |rescue
        |  puts "In rescue all"
        |ensure
        |  puts x
        |end
        |
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "Data flow for begin/rescue with data flow through the exception" should {
    val cpg = code("""
        |x = "Exception message: "
        |begin
        |1/0
        |rescue ZeroDivisionError => e
        |   y = x + e.message
        |   puts y
        |end
        |
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "Data flow for begin/rescue with data flow through block with multiple exceptions being caught" should {
    val cpg = code("""
        |x = 1
        |y = 10
        |begin
        |1/0
        |rescue SystemCallError, ZeroDivisionError
        |   y = x + 100
        |end
        |
        |puts y
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "Data flow for begin/rescue with sink in function without begin" should {
    val cpg = code("""
        |def foo(arg)
        |  puts "in begin"
        |rescue SomeException
        |  return arg
        |rescue => exvar
        |  puts "Caught exception in variable #{exvar}"
        |rescue
        |  puts "Catch-all block"
        |end
        |
        |x = 1
        |y = foo x
        |puts y
        |
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "Data flow for begin/rescue with sink in function without begin and sink in rescue with exception" should {
    val cpg = code("""
        |def foo(arg)
        |  puts "in begin"
        |rescue SomeException
        |  puts "SomeException occurred #{arg}"
        |rescue => exvar
        |  puts "Caught exception in variable #{exvar}"
        |rescue
        |  puts "Catch-all block"
        |end
        |
        |x = 1
        |foo x
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "Data flow for begin/rescue with sink in function without begin and sink in catch-call rescue" should {
    val cpg = code("""
        |def foo(arg)
        |  puts "in begin"
        |  raise "This is an exception"
        |rescue
        |  puts "Catch-all block. Arg is #{arg}"
        |end
        |
        |x = 1
        |foo x
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "Data flow for begin/rescue with sink in function without begin with return from begin" should {
    val cpg = code("""
        |def foo(arg)
        |  puts "in begin"
        |  return arg
        |rescue SomeException
        |  puts "Caught SomeException"
        |rescue => exvar
        |  puts "Caught exception in variable #{exvar}"
        |rescue
        |  puts "Catch-all block"
        |end
        |
        |x = 1
        |y = foo x
        |puts y
        |
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "Data flow for begin/rescue with sink in function within do block" ignore {
    val cpg = code("""
        |def foo(arg)
        |  puts "in begin"
        |  arg do |y|
        |  return y
        |rescue SomeException
        |  puts "Caught SomeException"
        |rescue => exvar
        |  puts "Caught exception in variable #{exvar}"
        |rescue
        |  puts "Catch-all block"
        |end
        |
        |x = 1
        |z = foo x
        |puts z
        |
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").lineNumber(8).l
      sink.reachableByFlows(source).size shouldBe 1
    }
  }

  "Data flow through array assignments" should {
    val cpg = code("""
        |x = 10
        |array = [0, 1]
        |array[0] = x
        |puts array
        |
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "Data flow through chained scoped constant reference" should {
    val cpg = code("""
        |module SomeModule
        |SomeConstant = 1
        |end
        |
        |x = 1
        |y = SomeModule::SomeConstant * x
        |puts y
        |
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "Data flow through scopedConstantAccessSingleLeftHandSide" should {
    val cpg = code("""
        |SomeConstant = 1
        |
        |x = 1
        |::SomeConstant = x
        |y = ::SomeConstant + 10
        |puts y
        |
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "Data flow through xdotySingleLeftHandSide through a constant on left of the ::" should {
    val cpg = code("""
        |module SomeModule
        |SomeConstant = 100
        |end
        |
        |x = 2
        |SomeModule::SomeConstant = x
        |y = SomeModule::SomeConstant
        |puts y
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  // TODO:
  "Data flow through xdotySingleLeftHandSide through a local on left of the ::" ignore {
    val cpg = code("""
        |module SomeModule
        |SomeConstant = 100
        |end
        |
        |x = 2
        |local = SomeModule
        |local::SomeConstant = x
        |y = SomeModule::SomeConstant
        |puts y
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "Data flow through packing left hand side through the first identifier" should {
    val cpg = code("""
        |x = 1
        |p = 2
        |*y = x,p
        |puts y
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "Data flow through packing left hand side through beyond the first identifier" should {
    val cpg = code("""
        |x = 1
        |y = 2
        |z = 3
        |*a = z,y,x
        |puts a
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "Data flow through packing left hand side with unequal RHS" should {
    val cpg = code("""
          |x = 1
          |y = 2
          |z = 3
          |p,*a = z,y,x
          |puts a
          |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "Data flow through single LHS and multiple RHS" should {
    val cpg = code("""
        |x = 1
        |y = 2
        |z = 3
        |a = z,y,x
        |puts a
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "Data flow through argsAndDoBlockAndMethodIdCommandWithDoBlock" should {
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

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "Data flow through primaryMethodArgsDoBlockCommandWithDoBlock" should {
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

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "Data flow with super usage" should {
    val cpg = code("""
          |class BaseClass
          |  def doSomething(arg)
          |    return arg + 10
          |  end
          |end
          |
          |class DerivedClass < BaseClass
          |  def doSomething(arg)
          |    super(arg)
          |  end
          |end
          |
          |x = 1
          |object = DerivedClass.new
          |y = object.doSomething(x)
          |puts y
          |
          |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "Data flow through blockExprAssocTypeArguments" should {
    val cpg = code("""
          |def foo(*args)
          |puts args
          |end
          |
          |x = "value1"
          |foo(key1: x, key2: "value2")
          |
          |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "Data flow through blockSplattingTypeArguments" should {
    val cpg = code("""
          |def foo(arg)
          |puts arg
          |end
          |
          |x = 1
          |foo(*x)
          |
          |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "Data flow through blockSplattingExprAssocTypeArguments without block" should {
    val cpg = code("""
          |def foo(*arg)
          |puts arg
          |end
          |
          |x = 1
          |foo( x+1, key1: x*2, key2: x*3 )
          |
          |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "Data flow through blockSplattingTypeArguments without block" should {
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

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  // TODO:
  "Data flow through blockExprAssocTypeArguments with block argument in the wrapper function" ignore {
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

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "Data flow through grouping expression with negation" should {
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

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "Data flow through break with args" should {
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

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 4
    }
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

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  // TODO:
  "Data flow through a global variable" ignore {
    val cpg = code("""
          |def foo(arg)
          | loop do
          | arg += 1
          |  if arg > 3
          |        $y = arg
          |        return
          |  end
          | end
          |end
          |
          |x = 1
          |foo x
          |puts $y
          |
          |
          |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "Data flow using a keyword" should {
    val cpg = code("""
          |class MyClass
          |end
          |
          |x = MyClass.new
          |y = x.class
          |puts y
          |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "Data flow through variable params" should {
    val cpg = code("""
          |def foo(*args)
          |  return args
          |end
          |
          |x = 1
          |y = foo(x, "another param")
          |puts y
          |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "Data flow through optional params" should {
    val cpg = code("""
          |def foo(arg=10)
          |  return arg + 10
          |end
          |
          |x = 1
          |y = foo(x)
          |puts y
          |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "Data flow across files" should {
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

    "be found in" in {
      val source = cpg.literal.code("1").l
      val sink   = cpg.call.name("puts").argument(1).l
      sink.reachableByFlows(source).size shouldBe 1
    }
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

    "be found in" in {
      val source = cpg.literal.code("1").l
      val sink   = cpg.call.name("puts").argument(1).lineNumber(3).l
      sink.reachableByFlows(source).size shouldBe 1
      val src = cpg.identifier("x").lineNumber(3).l
      sink.reachableByFlows(src).size shouldBe 1
    }

    // TODO: Need to be fixed.
    "be found for sink in nested block" ignore {
      val src  = cpg.identifier("x").lineNumber(3).l
      val sink = cpg.call.name("puts").argument(1).lineNumber(7).l
      sink.reachableByFlows(src).size shouldBe 1
    }
  }

  "Data flows for pseudo variable identifiers" should {
    "Data flow for __LINE__ variable identifier" should {
      val cpg = code("""
          |x=1
          |a=x+__LINE__
          |puts a
          |""".stripMargin)

      "find flows to the sink" in {
        val source = cpg.identifier.name("x").l
        val sink   = cpg.call.name("puts").l
        sink.reachableByFlows(source).size shouldBe 2
      }
    }
  }

  "Data flow for chained command with do-block with parentheses" should {
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

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "Data flow for chained command with do-block without parentheses" should {
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

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "Data flow for yield block specified alongwith the call" should {
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

    "find flows to the sink" in {
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

  "Data flows through range operators" should {
    val cpg = code("""
        |x = 10
        |y=0
        |for i in 1...10 do
        |   x += i
        |   if (x > 10)
        |     y = x
        |   end
        |end
        |
        |puts y
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 3
    }
  }

  "Data flow through unless modifier" should {
    val cpg = code("""
        |x = 1
        |
        |x += 2 unless x.zero?
        |    puts(x)
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 3
    }
  }

  "Data flow through invocation or command with EMARK" should {
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

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 3
    }
  }
  // TODO:
  "Data flow through overloaded operator method" ignore {
    val cpg = code("""
        |class Foo
        |    @@x = 1
        |    def +(y)
        |        @@x + y
        |    end
        |end
        |
        |y = Foo.new + 1
        |
        |puts y
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.member.name("@@x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 3
    }
  }

  // TODO:
  "Data flow through assignment-like method identifier" ignore {
    val cpg = code("""
        |class Foo
        |    @@x = 1
        |    def CONST=(y)
        |        return @@x == y
        |    end
        |end
        |puts Foo::CONST= 2
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.member.name("@@x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 3
    }
  }

  // TODO:
  "Data flow through a when argument context" ignore {
    val cpg = code("""
        |x = 10
        |
        |case x
        |
        |when 1..5
        |    y = x
        |when 5..10
        |    z = x
        |when 10..15
        |    w = x
        |else
        |    _p = x
        |end
        |
        |puts _p
        |puts w
        |puts y
        |puts z
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 3
    }
  }

  "Data flow through ensureClause" should {
    val cpg = code("""
        |begin
        |    x = File.open("myFile.txt", "r")
        |    x << "#{content} \n"
        |rescue
        |  x = "pqr"
        |ensure
        |  x = "abc"
        |  y = x
        |end
        |
        |puts y
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2 // flow through the rescue is not a flow
    }
  }

  "Data flow through begin-else" should {
    val cpg = code("""
        |begin
        |    x = File.open("myFile.txt", "r")
        |    x << "#{content} \n"
        |rescue
        |  x = "pqr"
        |else
        |  y = x
        |ensure
        |  x = "abc"
        |end
        |
        |puts y
        |""".stripMargin).moreCode(
      """
        |My file
        |""".stripMargin,
      "myFile.txt"
    )

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 3
    }
  }

  "Data flow through block argument context" should {
    val cpg = code("""
        |x=10
        |y=0
        |def foo(n, &block)
        |   woo(n, &block)
        |end
        |
        |def woo(n, &block)
        |    n.times {yield}
        |end
        |
        |foo(5) {
        |    y = x
        |}
        |
        |puts y
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "Data flow through block splatting type arguments context" should {
    val cpg = code("""
        |x=10
        |y=0
        |def foo(*n, &block)
        |   woo(*n, &block)
        |end
        |
        |def woo(n, &block)
        |    n.times {yield}
        |end
        |
        |foo(5) {
        |    y = x
        |}
        |
        |puts y
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "Flow through tainted object" should {
    val cpg = code("""
        |def put_req(api_endpoint, params)
        |    puts "Hitting " + api_endpoint + " with params: " + params
        |end
        |class TestClient
        |    def get_event_data(accountId)
        |        payload = accountId
        |        r = put_req(
        |            "https://localhost:8080/v3/users/me/",
        |            params=payload
        |        )
        |    end
        |end
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("accountId").l
      val sink   = cpg.call.name("put_req").l
      sink.reachableByFlows(source).size shouldBe 1
    }
  }

  // TODO:
  "Flow for a global variable" ignore {
    val cpg = code("""
        |$person_height = 6
        |class Person
        |    def height_in_cm
        |        puts $person_height * 30
        |    end
        |end
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("$person_height").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "Flow for nested puts calls" should {
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

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 5
    }
  }

  "Data flow through a keyword? named method usage" should {
    val cpg = code("""
        |x = 1
        |y = x.nil?
        |puts y
        |""".stripMargin)

    "be found" in {
      val src  = cpg.identifier.name("x").l
      val sink = cpg.call.name("puts").l
      sink.reachableByFlows(src).size shouldBe 2
    }
  }

  "Data flow through a keyword inside a association" should {
    val cpg = code("""
        |def foo(arg)
        |puts arg
        |end
        |
        |x = 1
        |foo if: x.nil?
        |""".stripMargin)

    "be found" in {
      val src  = cpg.identifier.name("x").l
      val sink = cpg.call.name("puts").l
      sink.reachableByFlows(src).size shouldBe 2
    }
  }

  "Data flow through a regex interpolation" should {
    val cpg = code(s"""
        |x="abc"
        |y=/x#{x}b/
        |puts y
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "flow through a regex interpolation with multiple expressions" should {
    val cpg = code("""
        |x="abc"
        |y=/x#{x}b#{x+'z'}b{x+'y'+'z'}w/
        |puts y
        |""".stripMargin)
    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 3
    }
  }

  "flow through a proc definition using Proc.new and flow originating within the proc" should {
    val cpg = code("""
        |y = Proc.new {
        |x=1
        |x
        |}
        |puts y
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "flow through a proc definition with non-empty block and zero parameters" ignore {
    val cpg = code("""
        |x=10
        |y = x
        |-> {
        |puts y
        |}.call
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "flow through a proc definition with non-empty block and non-zero parameters" should {
    val cpg = code("""
        |x=10
        |-> (arg){
        |puts arg
        |}.call(x)
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "flow through a method call with safe navigation operator with parantheses" should {
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

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "flow through a method call with safe navigation operator without parantheses" should {
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

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "flow through a method call present in next line, with the second line starting with `.`" should {
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

    "find flow to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 1
    }
  }

  "flow through a method call present in next line, with the first line ending with `.`" should {
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

    "find flow to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 1
    }
  }

  "flow through statement when regular expression literal passed after `when`" should {
    val cpg = code("""
        |x = 2
        |a = 2
        |
        |case a
        | when /^ch/
        |   b = x
        |   puts b
        |end
        |""".stripMargin)

    "find flows to the sink" in {

      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "flow through interpolated double-quoted string literal " should {
    val cpg = code("""
        |x = "foo"
        |y = :"bar #{x}"
        |puts y
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "flow through conditional return statement" should {
    val cpg = code("""
        |class Foo
        | def bar(value)
        |   j = 0
        |   return(value) unless j == 0
        | end
        |end
        |
        |x = 10
        |foo = Foo.new
        |y = foo.bar(x)
        |puts y
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "flow through statement with ternary operator with multiple line" in {
    val cpg = code("""
        |x = 2
        |y = 3
        |z = 4
        |
        |w = x == 2 ?
        | y
        | : z
        |puts y
        |""".stripMargin)

    val source = cpg.identifier.name("y").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  "flow through endless method" in {
    val cpg = code("""
        |def multiply(a,b) = a*b
        |x = 10
        |y = multiply(3,x)
        |puts y
        |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  "flow through symbol literal defined using \\:" should {
    val cpg = code("""
        |def foo(arg)
        |hash = {:y => arg}
        |puts hash
        |end
        |
        |x = 3
        |foo(x)
        |""".stripMargin)

    "find flows to the sink" in {
      val source = cpg.identifier.name("x").l
      val sink   = cpg.call.name("puts").l
      sink.reachableByFlows(source).size shouldBe 2
    }
  }

  "flow through %w array" in {
    val cpg = code("""
        |a = %w[b c]
        |puts a
        |""".stripMargin)

    val source     = cpg.literal.code("b").l
    val sink       = cpg.call.name("puts").l
    val List(flow) = sink.reachableByFlows(source).map(flowToResultPairs).distinct.sortBy(_.length).l
    flow shouldBe List(("[b, c]", 2), ("[b, c]", -1), ("a = %w[b c]", 2), ("puts a", 3))
  }

  "flow through hash containing splatting literal" in {
    val cpg = code("""
        |x={:y=>1}
        |z = {
        |**x
        |}
        |puts z
        |""".stripMargin)
    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  "dataflow in method defined under class << self block" ignore {
    //    Marked it ignored as flow from identifier "firstName" to call "puts" is missing
    val cpg = code("""
       class MyClass
        |
        |  class << self
        |    def printPII
        |      firstName="somename"
        |      puts "log PII #{firstName}"
        |    end
        |  end
        |end
        |
        |MyClass.printPII""".stripMargin)

    val source = cpg.identifier.name("firstName").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  "flow through association identifier" in {
    val cpg = code("""
        |def foo(a:)
        | puts a
        |end
        |
        |x =1
        |foo(a:x)
        |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  "flow through special prefix methods" in {
    /* We only check private_class_method here. The mechanism is similar to others:
     *     attr_reader
     *     attr_writer
     *     attr_accessor
     *     remove_method
     *     public_class_method
     *     private
     *     protected
     */
    val cpg = code("""
        |class Foo
        | z = 1
        | private_class_method def self.bar(x)
        |   x
        | end
        |
        | y = self.bar(z)
        | puts y
        |end
        |""".stripMargin)

    val source = cpg.identifier.name("z").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  "flow through %i array" in {
    val cpg = code("""
        |a = %i[b
        |    c]
        |puts a
        |""".stripMargin)

    val source     = cpg.literal.code("b").l
    val sink       = cpg.call.name("puts").l
    val List(flow) = sink.reachableByFlows(source).map(flowToResultPairs).distinct.sortBy(_.length).l
    flow shouldBe List(
      ("[b, c]", 2),
      ("[b, c]", -1),
      (
        """|a = %i[b
           |    c]""".stripMargin,
        2
      ),
      ("puts a", 4)
    )
  }

  "flow through array constructor using []" in {
    val cpg = code("""
        |x=1
        |y=x
        |z = Array[y,2]
        |puts "#{z}"
        |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  "flow through array constructor using [] and command in []" in {
    val cpg = code("""
        |def foo(arg)
        |return arg
        |end
        |
        |x=1
        |y=x
        |z = Array[foo y]
        |puts "#{z}"
        |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }
}
