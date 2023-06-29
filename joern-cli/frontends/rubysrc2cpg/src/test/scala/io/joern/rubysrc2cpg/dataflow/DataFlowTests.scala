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
    // TODO test a lot more multiple assignments
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

  "Data flow through class member" ignore {
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
      sink.reachableByFlows(source).l.size shouldBe 8
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

  "Data flow for begin/rescue with sink in function without begin" ignore {
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

  "Data flow for begin/rescue with sink in function without begin and sink in rescue with exception" ignore {
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

  "Data flow for begin/rescue with sink in function without begin and sink in catch-call rescue" ignore {
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

  "Data flow through xdotySingleLeftHandSide" should {
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
}
