package io.joern.rubysrc2cpg.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class ControlStructureTests extends RubyCode2CpgFixture(withPostProcessing = true, withDataFlow = true) {

  "flow through body of a `while-end` statement" in {
    val cpg = code("""
        |x = 1
        |y = 10
        |while y > 0 do
        |  y = y - x
        |end
        |puts x
        |""".stripMargin)

    val source = cpg.literal("1")
    val sink   = cpg.method.name("puts").callIn.argument
    val flows  = sink.reachableByFlows(source).l
    flows.map(flowToResultPairs).toSet shouldBe
      Set(List(("x = 1", 2), ("y - x", 5), ("puts x", 7)))
  }

  "flow through body of a `... while ...` statement" in {
    val cpg = code("""
        |x = 1
        |y = 10
        |y = y - x while y > 0
        |puts x
        |""".stripMargin)

    val source = cpg.literal("1")
    val sink   = cpg.method.name("puts").callIn.argument
    val flows  = sink.reachableByFlows(source)
    flows.map(flowToResultPairs).toSet shouldBe
      Set(List(("x = 1", 2), ("y - x", 4), ("puts x", 5)))
  }

  "flow through body of an `until-end` statement" in {
    val cpg = code("""
        |x = 1
        |y = 10
        |until y <= 0 do
        |  y = y - x
        |end
        |puts x
        |""".stripMargin)
    val source = cpg.literal("1")
    val sink   = cpg.method.name("puts").callIn.argument
    val flows  = sink.reachableByFlows(source)
    flows.map(flowToResultPairs).toSet shouldBe
      Set(List(("x = 1", 2), ("y - x", 5), ("puts x", 7)))
  }

  // Works in deprecated - does not parse in new frontend
  "flow in through until modifier" in {
    val cpg = code("""
                     |i = 0
                     |num = 5
                     |begin
                     |   num = i + 3
                     |end until i < num
                     |puts num
                     |""".stripMargin)

    val src  = cpg.identifier.name("i").l
    val sink = cpg.call.name("puts").l
    sink.reachableByFlows(src).size shouldBe 3
  }

  "flow through for loop" in {
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

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).l.size shouldBe 2
  }

  "flow through for loop simple" in {
    val cpg = code("""
                     |x = 0
                     |arr = [1,2,3,4,5]
                     |num = 0
                     |for i in arr do
                     |   num = x
                     |end
                     |puts num
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).l.size shouldBe 2
  }

  "flow through for and next AFTER statement" in {
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

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).l.size shouldBe 2
  }

  "flow through for and next BEFORE statement" in {
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

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).l.size shouldBe 2
  }

  "flow through for and redo AFTER statement" in {
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

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).l.size shouldBe 2
  }

  "flow through for and redo BEFORE statement" in {
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

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).l.size shouldBe 2
  }

  "flow through for and retry AFTER statement" in {
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

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).l.size shouldBe 2
  }

  "Data flow through for and retry BEFORE statement" in {
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

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).l.size shouldBe 2
  }

  "flow through the 1st branch of an `if-end` statement" in {
    val cpg = code("""
        |t = 100
        |y = 1
        |if true
        | y = y - t
        |end
        |puts t
        |""".stripMargin)
    val source = cpg.literal("100")
    val sink   = cpg.method.name("puts").callIn.argument
    val flows  = sink.reachableByFlows(source)
    flows.map(flowToResultPairs).toSet shouldBe
      Set(List(("t = 100", 2), ("y - t", 5), ("puts t", 7)))
  }

  "flow through the 2nd branch of an `if-else-end` statement" in {
    val cpg = code("""
        |t = 100
        |if false
        | foo
        |else
        | t = t - 1
        |end
        |puts t
        |""".stripMargin)
    val source = cpg.literal("100")
    val sink   = cpg.method.name("puts").callIn.argument
    val flows  = sink.reachableByFlows(source)
    flows.map(flowToResultPairs).toSet shouldBe
      Set(List(("t = 100", 2), ("t - 1", 6), ("t = t - 1", 6), ("puts t", 8)))
  }

  "flow through the 2nd branch of an `if-elsif-end` statement" in {
    val cpg = code("""
        |t = 100
        |if false
        | foo
        |elsif true
        | t = t * 2
        |end
        |puts t
        |""".stripMargin)
    val source = cpg.literal("100")
    val sink   = cpg.method.name("puts").callIn.argument
    val flows  = sink.reachableByFlows(source)
    flows.map(flowToResultPairs).toSet shouldBe
      Set(List(("t = 100", 2), ("t * 2", 6), ("t = t * 2", 6), ("puts t", 8)))
  }

  "flow through both branches of an `if-else-end` statement" in {
    val cpg = code("""
        |t = 100
        |if false
        | puts t + 1
        |else
        | puts t + 2
        |end
        |""".stripMargin)
    val source = cpg.literal("100")
    val sink   = cpg.method.name("puts").callIn.argument
    val flows  = sink.reachableByFlows(source)
    flows.map(flowToResultPairs).toSet shouldBe
      Set(
        List(("t = 100", 2), ("t + 1", 4), ("puts t + 1", 4)),
        List(("t = 100", 2), ("t + 2", 6), ("puts t + 2", 6)),
        List(("t = 100", 2), ("t + 2", 6)),
        List(("t = 100", 2), ("t + 1", 4))
      )
  }

  "flow through an `unless-end` statement" in {
    val cpg = code("""
        |x = 1
        |unless __LINE__ == 0 then
        |  x = x * 2
        |end
        |puts x
        |""".stripMargin)
    val source = cpg.literal("1")
    val sink   = cpg.method.name("puts").callIn.argument
    val flows  = sink.reachableByFlows(source)
    flows.map(flowToResultPairs).toSet shouldBe
      Set(List(("x = 1", 2), ("x * 2", 4), ("x = x * 2", 4), ("puts x", 6)))
  }

  "flow through a `begin-rescue-end` expression" ignore {
    val cpg = code("""
        |x = 1
        |y = begin
        | x
        |rescue
        | x
        |end
        |puts y
        |""".stripMargin)
    val source = cpg.literal("1")
    val sink   = cpg.method.name("puts").callIn.argument
    val flows  = sink.reachableByFlows(source)
    flows.map(flowToResultPairs).toSet should not be empty
  }

  // Works in deprecated
  "Data flow for begin/rescue with sink in begin" ignore {
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

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
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

  "flow for begin/rescue with sink in rescue" in {
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

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  "Data flow for begin/rescue with sink in rescue with exception var" in {
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

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 1
  }

  "Data flow for begin/rescue with sink in catch-all rescue" in {
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

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  "Data flow for begin/rescue with sink in ensure" in {
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

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  "Data flow for begin/rescue with data flow through the exception" in {
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

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  "Data flow for begin/rescue with data flow through block with multiple exceptions being caught" in {
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

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  // Works in deprecated
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

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  // Works in deprecated
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

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  // Works in deprecated
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

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  // Works in deprecated
  "Data flow for begin/rescue with sink in function without begin with return from begin" ignore {
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

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
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

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  // Works in deprecated
  "Data flow through unless modifier" ignore {
    val cpg = code("""
                     |x = 1
                     |
                     |x += 2 unless x.zero?
                     |    puts(x)
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 3
  }

  "Data flow through ensureClause" in {
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

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2 // flow through the rescue is not a flow
  }

  // Works in deprecated
  "Data flow through begin-else" ignore {
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

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 3
  }

  // Works in deprecated
  "flow through conditional return statement" ignore {
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

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

}
