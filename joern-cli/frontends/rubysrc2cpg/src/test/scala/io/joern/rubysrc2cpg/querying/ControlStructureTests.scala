package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language._

class ControlStructureTests extends RubyCode2CpgFixture {

  "CPG for code with doBlock iterating over a constant array" should {
    val cpg = code("""
          |[1, 2, "three"].each do |n|
          | puts n
          |end
          |""".stripMargin)

    "recognise all identifier nodes" in {
      cpg.identifier.name("n").size shouldBe 1
      cpg.identifier.size shouldBe 1
    }

    "recognize all call nodes" in {
      // cpg.call.name("each").size shouldBe 1
      cpg.call.name("puts").size shouldBe 1
    }
  }

  "CPG for code with doBlock iterating over a constant array and multiple params" should {
    val cpg = code("""
          |[1, 2, "three"].each do |n, m|
          |
          |
          |  expect {
          |  someObject.someMethod(n)
          |  someObject.someMethod(m)
          |  }.to otherMethod(n).by(1)
          |end
          |
          |""".stripMargin)

    "recognise all identifier nodes" in {
      cpg.identifier.name("n").size shouldBe 2
      cpg.identifier.name("m").size shouldBe 1
      cpg.identifier.size shouldBe 5
      cpg.method.name("fakeName").dotAst.l
    }

    "recognize all call nodes" in {
      // cpg.call.name("each").size shouldBe 1
      cpg.call.name("someMethod").size shouldBe 2
      cpg.call.name("expect").size shouldBe 1
      cpg.call.name("to").size shouldBe 1
      cpg.call.name("otherMethod").size shouldBe 1
      cpg.call.name("by").size shouldBe 1
    }
  }

  "CPG for code with return having an if statement" should {
    val cpg = code("""
          |def some_method
          |  return if some_var
          |end
          |
          |""".stripMargin)

    /*
     * This code used jumpExpression. This validated t
     */
    "recognise identifier nodes in the jump statement" in {
      cpg.identifier.name("some_var").size shouldBe 1
    }

    "identify the control structure code" in {
      cpg.controlStructure.code("return if some_var").size shouldBe 1
    }
  }

  "CPG for code with yield" should {
    val cpg = code("""
        |def yield_with_args_method
        |   yield 2*3
        |   yield 100
        |   yield
        |end
        |
        |yield_with_args_method {|i| puts "arg is #{i}"}
        |
        |""".stripMargin)

    "recognise all method nodes" in {
      cpg.method.name("yield_with_args_method").size shouldBe 1
      // TODO need to figure out how yield block should be connected to the method
    }
  }

  "CPG for code with if/else condition" should {
    val cpg = code("""
        |x = 1
        |if x > 2
        |   puts "x is greater than 2"
        |elsif x <= 2 and x!=0
        |   puts "x is 1"
        |else
        |   puts "I can't guess the number"
        |end
        |
        |""".stripMargin)

    "recognise all identifier nodes" in {
      cpg.identifier.name("x").size shouldBe 4
    }

    "recognize all literal nodes" in {
      cpg.literal.code("1").size shouldBe 1
      cpg.literal.code("2").size shouldBe 2
      cpg.literal.code("0").size shouldBe 1
      cpg.literal.code("\"x is 1\"").size shouldBe 1
      cpg.literal.code("\"I can't guess the number\"").size shouldBe 1
    }
  }

  "CPG for code with conditional operator" should {
    val cpg = code("""
          |y = ( x > 2 ) ? x : x + 1
          |""".stripMargin)

    "recognise all literal and identifier nodes" in {
      cpg.identifier.name("x").size shouldBe 3
      cpg.identifier.name("y").size shouldBe 1
      cpg.literal.code("1").size shouldBe 1
    }
  }

  "CPG for code with unless condition" should {
    val cpg = code("""
        |x = 1
        |unless x > 2
        |   puts "x is less than or equal to 2"
        |else
        |   puts "x is greater than 2"
        |end
        |
        |""".stripMargin)

    "recognise all literal nodes" in {
      cpg.identifier.name("x").size shouldBe 2
      cpg.literal.code("2").size shouldBe 1
      cpg.literal.code("\"x is less than or equal to 2\"").size shouldBe 1
      cpg.literal.code("\"x is greater than 2\"").size shouldBe 1
    }

    "recognise all call nodes" in {
      cpg.call.name("puts").size shouldBe 2
    }
  }

  "CPG for code with case statement and case argument" should {
    val cpg = code("""
        |choice = "5"
        |case choice
        |when "1","2"
        |        puts "1 or 2"
        |when "3","4"
        |        puts "3 or 4"
        |when "5","6"
        |        puts "5 or 6"
        |when "7","8"
        |        puts "7 or 8"
        |else
        |    "No match"
        |end
        |
        |""".stripMargin)

    "recognise all literal nodes" in {
      cpg.identifier.name("choice").size shouldBe 2
      cpg.literal.code("\"1\"").size shouldBe 1
      cpg.literal.code("\"2\"").size shouldBe 1
      cpg.literal.code("\"3\"").size shouldBe 1
      cpg.literal.code("\"4\"").size shouldBe 1
      cpg.literal.code("\"5\"").size shouldBe 2
      cpg.literal.code("\"6\"").size shouldBe 1
      cpg.literal.code("\"7\"").size shouldBe 1
      cpg.literal.code("\"8\"").size shouldBe 1
      cpg.literal.code("\"1 or 2\"").size shouldBe 1
      cpg.literal.code("\"3 or 4\"").size shouldBe 1
      cpg.literal.code("\"5 or 6\"").size shouldBe 1
      cpg.literal.code("\"7 or 8\"").size shouldBe 1
    }

    "recognise all call nodes" in {
      cpg.call.name("puts").size shouldBe 4
    }
  }

  "CPG for code with case statement and no case" should {
    val cpg = code("""
        |str = "some_string"
        |
        |case
        |when str.match('/\d/')
        |    puts 'String contains numbers'
        |when str.match('/[a-zA-Z]/')
        |    puts 'String contains letters'
        |else
        |    puts 'String does not contain numbers & letters'
        |end
        |
        |""".stripMargin)

    "recognise all literal nodes" in {
      cpg.identifier.name("str").size shouldBe 3
      cpg.literal.code("\"some_string\"").size shouldBe 1
      cpg.literal.code("'String contains numbers'").size shouldBe 1
      cpg.literal.code("'String contains letters'").size shouldBe 1
      cpg.literal.code("'String does not contain numbers & letters'").size shouldBe 1
    }

    "recognise all call nodes" in {
      cpg.call.name("puts").size shouldBe 3
    }
  }

  "CPG for code with a while loop" should {
    val cpg = code("""
        |x = 10
        |while x >= 1
        |  x = x - 1
        |  puts "In the loop"
        |end
        |""".stripMargin)

    "recognise all method nodes" in {
      cpg.identifier
        .name("x")
        .size shouldBe 4 // FIXME this shows as 3 when the puts is the first loop statemnt. Find why
      cpg.literal.code("\"In the loop\"").size shouldBe 1
    }

    "recognise all call nodes" in {
      cpg.call.name("puts").size shouldBe 1
    }
  }

  "CPG for code with a until loop" should {
    val cpg = code("""
        |x = 10
        |until x == 0
        |  x = x - 1
        |  puts "In the loop"
        |end
        |""".stripMargin)

    "recognise all method nodes" in {
      cpg.identifier
        .name("x")
        .size shouldBe 4 // FIXME this shows as 3 when the puts is the first loop statemnt. Find why
      cpg.literal.code("\"In the loop\"").size shouldBe 1
    }

    "recognise all call nodes" in {
      cpg.call.name("puts").size shouldBe 1
    }
  }

  "CPG for code with a for loop" should {
    val cpg = code("""
        |for x in 1..10 do
        |  puts x
        |end
        |""".stripMargin)

    "recognise all literal nodes" in {
      cpg.identifier
        .name("x")
        .size shouldBe 2
      cpg.literal.code("1").size shouldBe 1
      cpg.literal.code("10").size shouldBe 1

    }

    "recognise all call nodes" in {
      cpg.call.name("puts").size shouldBe 1
    }
  }

  "CPG for code with modifier statements" should {
    val cpg = code("""
        |for i in 1..10
        |  next if i % 2 == 0
        |  redo if i > 8
        |  retry if i > 7
        |  puts i if i == 9
        |  i += 4 unless i > 5
        |
        |  value1 = 0
        |  value1 += 1 while value1 < 100
        |
        |  value2 = 0
        |  value2 += 1 until value2 >= 100
        |end
        |""".stripMargin)

    "recognise all identifier nodes" in {
      cpg.identifier
        .name("i")
        .size shouldBe 8
      cpg.identifier
        .name("value1")
        .size shouldBe 3
      cpg.identifier
        .name("value2")
        .size shouldBe 3
    }

    "recognize all literal nodes" in {
      cpg.literal.code("1").size shouldBe 3
      cpg.literal.code("2").size shouldBe 1
      cpg.literal.code("8").size shouldBe 1
      cpg.literal.code("7").size shouldBe 1
      cpg.literal.code("9").size shouldBe 1
      cpg.literal.code("5").size shouldBe 1
      cpg.literal.code("0").size shouldBe 3
      cpg.literal.code("1").size shouldBe 3
      cpg.literal.code("10").size shouldBe 1
      cpg.literal.code("100").size shouldBe 2
    }

    "recognise all call nodes" in {
      cpg.call.name("puts").size shouldBe 1
    }
  }
}
