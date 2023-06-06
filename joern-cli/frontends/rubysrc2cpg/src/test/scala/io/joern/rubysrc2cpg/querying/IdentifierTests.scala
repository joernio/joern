package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language._

class IdentifierTests extends RubyCode2CpgFixture {

  "CPG for code with method identifiers and literals in simple assignments" should {
    val cpg = code("""
        |# call instance methods
        |a = 1
        |b = 2
        |a = 3
        |b = 4
        |c = a*b
        |puts "Multiplication is : #{c}"
        |""".stripMargin)

    "recognise all identifier nodes" in {
      cpg.identifier.name("a").l.size shouldBe 3
      cpg.identifier.name("b").l.size shouldBe 3
      cpg.identifier.name("c").l.size shouldBe 2
    }

    "recognise all literal nodes" in {
      cpg.literal.code("1").l.size shouldBe 1
      cpg.literal.code("2").l.size shouldBe 1
      cpg.literal.code("3").l.size shouldBe 1
      cpg.literal.code("4").l.size shouldBe 1
    }

    "successfully plot ASTs" in {
      cpg.method.name(":program").dotAst.l
    }
  }

  "CPG for code with class methods, members and locals in methods" should {
    val cpg = code("""
        |class Person
        |  attr_accessor :name, :age
        |
        |  def initialize(name, age)
        |    @name = name
        |    @age = age
        |  end
        |
        |  def greet
        |    puts "Hello, my name is #{@name} and I am #{@age} years old."
        |  end
        |
        |  def have_birthday
        |    @age += 1
        |    puts "Happy birthday! You are now #{@age} years old."
        |  end
        |end
        |
        |p = Person. new
        |p.greet
        |""".stripMargin)

    "recognise all identifier and call nodes" in {
      cpg.identifier.name("name").l.size shouldBe 1
      cpg.identifier.name("age").l.size shouldBe 1
      cpg.identifier.name("@name").l.size shouldBe 2
      cpg.identifier.name("@age").l.size shouldBe 4
      cpg.call.name("greet").size shouldBe 1
      cpg.method.name("initialize").size shouldBe 1
      cpg.method
        .name("greet")
        .size shouldBe 1
      cpg.call.name("puts").size shouldBe 2
      cpg.method.name("have_birthday").size shouldBe 1
      cpg.identifier.size shouldBe 11
    }

    "successfully plot ASTs" in {
      cpg.method.name(":program").dotAst.l
      cpg.method.name("initialize").dotAst.l
      cpg.method.name("greet").dotAst.l
      cpg.method.name("have_birthday").dotAst.l
    }
  }

  "CPG for code with a function call, arguments and function called from function " should {
    val cpg = code("""
        |
        |def extrareturn()
        |  ret = 6
        |  return ret
        |end
        |
        |def add_three_numbers(num1, num2, num3)
        |  sum = num1 + num2 + num3 + extrareturn()
        |  return sum
        |end
        |
        |a = 1
        |b = 2
        |c = 3
        |
        |sumOfThree = add_three_numbers( a, b, c )
        |""".stripMargin)

    "recognise all identifier and call nodes" in {
      cpg.identifier.name("a").l.size shouldBe 2
      cpg.identifier.name("b").l.size shouldBe 2
      cpg.identifier.name("c").l.size shouldBe 2
      cpg.identifier.name("sumOfThree").l.size shouldBe 1
      cpg.identifier.name("num1").l.size shouldBe 1
      cpg.identifier.name("num2").l.size shouldBe 1
      cpg.identifier.name("num3").l.size shouldBe 1
      cpg.identifier.name("sum").l.size shouldBe 2
      cpg.identifier.name("ret").l.size shouldBe 2
      cpg.call.name("add_three_numbers").size shouldBe 1
      cpg.method.name("add_three_numbers").dotAst.l
      cpg.identifier.size shouldBe 14
    }

    "successfully plot ASTs" in {
      cpg.method.name(":program").dotAst.l
      cpg.method.name("add_three_numbers").dotAst.l
      cpg.method.name("extrareturn").dotAst.l
    }
  }
  "CPG for code with expressions of various types" should {
    val cpg = code("""
          |a = 1
          |b = 2 if a > 1
          |b = !a
          |c = ~a
          |e = +a
          |f = b**a
          |g = a*b
          |h = a+b
          |i = a >> b
          |j = a | b
          |k = a & b
          |l = a && b
          |m = a || b
          |n = a .. b
          |o = a ... b
          |p = ( a > b ) ? c : e
          |q = not p
          |r = p and q
          |s = p or q
          |""".stripMargin)

    "recognise all identifier nodes" in {
      cpg.identifier.name("a").l.size shouldBe 16
      cpg.identifier.name("b").l.size shouldBe 13 // unaryExpression
      cpg.identifier.name("c").l.size shouldBe 2  // unaryExpression
      cpg.identifier.name("e").l.size shouldBe 2  // unaryExpression
      cpg.identifier.name("f").l.size shouldBe 1  // powerExpression
      cpg.identifier.name("g").l.size shouldBe 1  // multiplicative Expression
      cpg.identifier.name("h").l.size shouldBe 1  // additive Expression
      cpg.identifier.name("i").l.size shouldBe 1  // bitwise shift Expression
      cpg.identifier.name("j").l.size shouldBe 1  // bitwise or Expression
      cpg.identifier.name("k").l.size shouldBe 1  // bitwise and Expression
      cpg.identifier.name("l").l.size shouldBe 1  // operator and Expression
      cpg.identifier.name("m").l.size shouldBe 1  // operator or Expression
      cpg.identifier.name("n").l.size shouldBe 1  // inclusive range Expression
      cpg.identifier.name("o").l.size shouldBe 1  // exclusive range Expression
      cpg.identifier.name("p").l.size shouldBe 4  // conditionalOperatorExpression
      cpg.identifier.name("q").l.size shouldBe 3  // notExpressionOrCommand
      cpg.identifier.name("r").l.size shouldBe 1  // orAndExpressionOrCommand and part
      cpg.identifier.name("s").l.size shouldBe 1  // orAndExpressionOrCommand or part
      cpg.identifier.size shouldBe 52
    }

    "successfully plot ASTs" in {
      cpg.method.name(":program").dotAst.l
    }
  }

  "CPG for code with BEGIN and END blocks" should {
    val cpg = code("""
          |#!/usr/bin/env ruby
          |
          |# This code block will be executed before the program begins
          |BEGIN {
          |  beginvar = 5
          |  beginbool = beginvar > 21
          |}
          |
          |# This is the main logic of the program
          |puts "Hello, world!"
          |
          |# This code block will be executed after the program finishes
          |END {
          |  endvar = 67
          |  endbool = endvar > 23
          |}
          |""".stripMargin)

    "recognise all identifier and call nodes" in {
      cpg.identifier.name("beginvar").l.size shouldBe 2
      cpg.identifier.name("endvar").l.size shouldBe 2
      cpg.identifier.name("beginbool").l.size shouldBe 1
      cpg.identifier.name("endbool").l.size shouldBe 1
      cpg.call.name("puts").size shouldBe 1
      cpg.identifier.size shouldBe 6
    }

    "successfully plot ASTs" in {
      cpg.method.name(":program").dotAst.l
    }
  }

  "CPG for code with doBlock iterating over a constant array" should {
    val cpg = code("""
          |[1, 2, "three"].each do |n|
          | puts n
          |end
          |""".stripMargin)

    "recognise all identifier nodes" in {
      cpg.identifier.name("n").l.size shouldBe 2
      cpg.call.name("each").size shouldBe 1
      cpg.call.name("puts").size shouldBe 1
      cpg.identifier.size shouldBe 2
    }

    "successfully plot ASTs" in {
      cpg.method.name(":program").dotAst.l
    }
  }

  "CPG for code with doBlock iterating over a constant array and multiple params" should {
    val cpg = code("""
          |[1, 2, "three"].each do |n, m|
          |  expect {
          |  someObject.someMethod(n)
          |  someObject.someMethod(m)
          |  }.to otherMethod(n).by(1)
          |end
          |
          |""".stripMargin)

    "recognise all identifier and call nodes" in {
      cpg.identifier.name("n").l.size shouldBe 3
      cpg.identifier.name("m").l.size shouldBe 2
      cpg.call.name("each").size shouldBe 1
      cpg.call.name("someMethod").size shouldBe 2
      cpg.call.name("expect").size shouldBe 1
      cpg.call.name("to").size shouldBe 1
      cpg.call.name("otherMethod").size shouldBe 1
      cpg.call.name("by").size shouldBe 1
      cpg.identifier.size shouldBe 7
    }

    "successfully plot ASTs" in {
      cpg.method.name(":program").dotAst.l
    }
  }

  "CPG for code with namespace resolution being used" should {
    val cpg = code("""
          |Rails.application.configure do
          |  config.log_formatter = ::Logger::Formatter.new
          |end
          |
          |""".stripMargin)

    "recognise all identifier and call nodes" in {
      cpg.call.name("application").size shouldBe 1
      cpg.call.name("configure").size shouldBe 1
      cpg.call.name("new").size shouldBe 1
      cpg.identifier.name("Rails").l.size shouldBe 1
      cpg.identifier.name("config").l.size shouldBe 1
      cpg.identifier.name("Formatter").l.size shouldBe 1
      cpg.identifier.name("Logger").l.size shouldBe 1
      cpg.identifier.name("log_formatter").l.size shouldBe 1
      cpg.identifier.size shouldBe 5
    }

    "successfully plot ASTs" in {
      cpg.method.name(":program").dotAst.l
    }
  }

  "CPG for code with square brackets as methods" should {
    val cpg = code("""
          |class MyClass < MyBaseClass
          |  def initialize
          |    @my_hash = {}
          |  end
          |
          |  def [](key)
          |    @my_hash[key.to_s]
          |  end
          |
          |  def []=(key, value)
          |    @my_hash[key.to_s] = value
          |  end
          |end
          |
          |my_object = MyClass.new
          |
          |""".stripMargin)

    "recognise all identifier and call nodes" in {
      cpg.method.name("\\[]").size shouldBe 2
      cpg.method.name("\\[]=").size shouldBe 1
      cpg.call.name("<operator>.assignment").size shouldBe 3
      cpg.method.name("initialize").size shouldBe 1
      cpg.call.name("to_s").size shouldBe 2
      cpg.call.name("new").size shouldBe 1
      cpg.call.size shouldBe 9
      cpg.identifier.name("@my_hash").size shouldBe 3
      cpg.identifier.name("key").size shouldBe 2
      cpg.identifier.name("value").size shouldBe 1
      cpg.identifier.name("my_object").size shouldBe 1
      /*
       * FIXME
       *  def []=(key, value) gets parsed incorrectly with parser error "no viable alternative at input 'def []=(key, value)'"
       *  This needs a fix in the parser and update to this UT after the fix
       * FIXME
       *  MyClass is identified as a variableIdentifier and so an identifier. This needs to be fixed
       */
    }

    "successfully plot ASTs" in {
      cpg.method.name(":program").dotAst.l
      cpg.method.name("\\[]=").dotAst.l
      cpg.method.name("\\[]=").dotAst.l
    }
  }

  "CPG for code with return having a if statement" should {
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
      cpg.controlStructure.code("return if some_var").size shouldBe 1
    }

    "successfully plot ASTs" in {
      cpg.method.name(":program").dotAst.l
      cpg.method.name("some_method").dotAst.l
    }
  }

  "CPG for code with defined? keyword" should {
    val cpg = code("""
        |radius = 2
        |
        |area = 3.14 * radius * radius
        |
        |# Checking if the variable is defined or not
        |# Using defined? keyword
        |res1 = defined? radius
        |res2 = defined? height
        |res3 = defined? area
        |res4 = defined? Math::PI
        |
        |# Displaying results
        |puts "Result 1: #{res1}"
        |puts "Result 2: #{res2}"
        |puts "Result 3: #{res3}"
        |puts "Result 4: #{res4}"
        |""".stripMargin)

    "recognise all identifier nodes" in {
      cpg.identifier.name("radius").l.size shouldBe 4
      cpg.identifier.name("area").l.size shouldBe 2
      cpg.identifier.name("height").l.size shouldBe 1
      cpg.identifier.name("res1").l.size shouldBe 2
      cpg.identifier.name("res2").l.size shouldBe 2
      cpg.identifier.name("res3").l.size shouldBe 2
      cpg.identifier.name("res4").l.size shouldBe 2
      cpg.identifier.name("Math").l.size shouldBe 1
      cpg.identifier.name("PI").l.size shouldBe 1
    }

    "recognise all literal nodes" in {
      cpg.literal.code("3.14").l.size shouldBe 1
      cpg.literal.code("2").l.size shouldBe 1
      cpg.literal.code("Result 1: ").l.size shouldBe 1
      cpg.literal.code("Result 2: ").l.size shouldBe 1
      cpg.literal.code("Result 3: ").l.size shouldBe 1
      cpg.literal.code("Result 4: ").l.size shouldBe 1
    }

    "successfully plot ASTs" in {
      cpg.method.name(":program").dotAst.l
    }
  }

  "CPG for code with multiple assignments" should {
    val cpg = code("""
        |a, b, c = [1, 2, 3]
        |a, b, c = b, c, a
        |str1, str2 = ["hello", "world"]
        |""".stripMargin)

    "recognise all identifier nodes" in {
      cpg.identifier.name("a").l.size shouldBe 3
      cpg.identifier.name("b").l.size shouldBe 3
      cpg.identifier.name("c").l.size shouldBe 3
      cpg.identifier.name("str1").l.size shouldBe 1
      cpg.identifier.name("str2").l.size shouldBe 1
    }

    "recognise all literal nodes" in {
      cpg.literal.code("1").l.size shouldBe 1
      cpg.literal.code("2").l.size shouldBe 1
      cpg.literal.code("3").l.size shouldBe 1
      cpg.literal.code("\"hello\"").l.size shouldBe 1
      cpg.literal.code("\"world\"").l.size shouldBe 1
    }

    "recognise all call nodes" in {
      cpg.call.name("<operator>.assignment").l.size shouldBe 3
    }

    "successfully plot ASTs" in {
      cpg.method.name(":program").dotAst.l
    }
  }

  "CPG for code with modules" should {
    val cpg = code("""
        |module Module1
        |   def method1_1
        |   end
        |   def method1_2
        |   end
        |end
        |
        |module Module2
        |   def method2_1
        |   end
        |   def method2_2
        |   end
        |end
        |""".stripMargin)

    "recognise all method nodes" in {
      cpg.method.name("method1_1").l.size shouldBe 1
      cpg.method.name("method1_2").l.size shouldBe 1
      cpg.method.name("method2_1").l.size shouldBe 1
      cpg.method.name("method2_2").l.size shouldBe 1
    }

    "successfully plot ASTs" in {
      cpg.method.name(":program").dotAst.l
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
      cpg.method.name("yield_with_args_method").l.size shouldBe 1
      // TODO need to figure out how yield block should be connected to the method
    }

    "successfully plot ASTs" in {
      cpg.method.name(":program").dotAst.l
      cpg.method.name("yield_with_args_method").dotAst.l
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

    "recognise all literal and identifier nodes" in {
      cpg.identifier.name("x").l.size shouldBe 4
      cpg.literal.code("1").l.size shouldBe 1
      cpg.literal.code("2").l.size shouldBe 2
      cpg.literal.code("0").l.size shouldBe 1
      cpg.literal.code("\"x is 1\"").l.size shouldBe 1
      cpg.literal.code("\"I can't guess the number\"").l.size shouldBe 1
    }
  }

  "CPG for code with conditional operator" should {
    val cpg = code("""
          |y = ( x > 2 ) ? x : x + 1
          |""".stripMargin)

    "recognise all literal and identifier nodes" in {
      cpg.identifier.name("x").l.size shouldBe 3
      cpg.identifier.name("y").l.size shouldBe 1
      cpg.literal.code("1").l.size shouldBe 1
    }

    "successfully plot ASTs" in {
      cpg.method.name(":program").dotAst.l
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
      cpg.identifier.name("x").l.size shouldBe 2
      cpg.literal.code("2").l.size shouldBe 1
      cpg.literal.code("\"x is less than or equal to 2\"").l.size shouldBe 1
      cpg.literal.code("\"x is greater than 2\"").l.size shouldBe 1
    }

    "recognise all call nodes" in {
      cpg.call.name("puts").l.size shouldBe 2
    }

    "successfully plot ASTs" in {
      cpg.method.name(":program").dotAst.l
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
      cpg.identifier.name("choice").l.size shouldBe 2
      cpg.literal.code("\"1\"").l.size shouldBe 1
      cpg.literal.code("\"2\"").l.size shouldBe 1
      cpg.literal.code("\"3\"").l.size shouldBe 1
      cpg.literal.code("\"4\"").l.size shouldBe 1
      cpg.literal.code("\"5\"").l.size shouldBe 2
      cpg.literal.code("\"6\"").l.size shouldBe 1
      cpg.literal.code("\"7\"").l.size shouldBe 1
      cpg.literal.code("\"8\"").l.size shouldBe 1
      cpg.literal.code("\"1 or 2\"").l.size shouldBe 1
      cpg.literal.code("\"3 or 4\"").l.size shouldBe 1
      cpg.literal.code("\"5 or 6\"").l.size shouldBe 1
      cpg.literal.code("\"7 or 8\"").l.size shouldBe 1
    }

    "recognise all call nodes" in {
      cpg.call.name("puts").l.size shouldBe 4
    }

    "successfully plot ASTs" in {
      cpg.method.name(":program").dotAst.l
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
      cpg.identifier.name("str").l.size shouldBe 3
      cpg.literal.code("\"some_string\"").l.size shouldBe 1
      cpg.literal.code("'String contains numbers'").l.size shouldBe 1
      cpg.literal.code("'String contains letters'").l.size shouldBe 1
      cpg.literal.code("'String does not contain numbers & letters'").l.size shouldBe 1
    }

    "recognise all call nodes" in {
      cpg.call.name("puts").l.size shouldBe 3
    }

    "successfully plot ASTs" in {
      cpg.method.name(":program").dotAst.l
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
        .l
        .size shouldBe 4 // FIXME this shows as 3 when the puts is the first loop statemnt. Find why
      cpg.literal.code("\"In the loop\"").l.size shouldBe 1
    }

    "recognise all call nodes" in {
      cpg.call.name("puts").l.size shouldBe 1
    }

    "successfully plot ASTs" in {
      cpg.method.name(":program").dotAst.l
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
        .l
        .size shouldBe 4 // FIXME this shows as 3 when the puts is the first loop statemnt. Find why
      cpg.literal.code("\"In the loop\"").l.size shouldBe 1
    }

    "recognise all call nodes" in {
      cpg.call.name("puts").l.size shouldBe 1
    }

    "successfully plot ASTs" in {
      cpg.method.name(":program").dotAst.l
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
        .l
        .size shouldBe 2
      cpg.literal.code("1").l.size shouldBe 1
      cpg.literal.code("10").l.size shouldBe 1

    }

    "recognise all call nodes" in {
      cpg.call.name("puts").l.size shouldBe 1
    }

    "successfully plot ASTs" in {
      cpg.method.name(":program").dotAst.l
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

    "recognise all literal nodes" in {
      cpg.identifier
        .name("i")
        .l
        .size shouldBe 8
      cpg.identifier
        .name("value1")
        .l
        .size shouldBe 3
      cpg.identifier
        .name("value2")
        .l
        .size shouldBe 3
      cpg.literal.code("1").l.size shouldBe 3
      cpg.literal.code("2").l.size shouldBe 1
      cpg.literal.code("8").l.size shouldBe 1
      cpg.literal.code("7").l.size shouldBe 1
      cpg.literal.code("9").l.size shouldBe 1
      cpg.literal.code("5").l.size shouldBe 1
      cpg.literal.code("0").l.size shouldBe 3
      cpg.literal.code("1").l.size shouldBe 3
      cpg.literal.code("10").l.size shouldBe 1
      cpg.literal.code("100").l.size shouldBe 2
    }

    "recognise all call nodes" in {
      cpg.call.name("puts").l.size shouldBe 1
    }

    "successfully plot ASTs" in {
      cpg.method.name(":program").dotAst.l
    }
  }

  "CPG for code with association statements" should {
    val cpg = code("""
        |class Employee < EmployeeBase
        |    has_many :teams, foreign_key: "team_id", class_name: "Team"
        |    has_many :worklocations, foreign_key: "location_id", class_name: "WorkLocation"
        |end
        |""".stripMargin)

    "recognise all literal nodes" in {
      cpg.literal
        .code("\"team_id\"")
        .l
        .size shouldBe 1
      cpg.literal
        .code("\"location_id\"")
        .l
        .size shouldBe 1
    }

    "successfully plot ASTs" in {
      cpg.method.name(":program").dotAst.l
    }
  }

  "CPG for code with keyword" should {
    val cpg = code("""
        |class User
        |  def attributes
        |    self.class.attributes
        |  end
        |end
        |
        |""".stripMargin)

    "successfully plot ASTs" in {
      cpg.method.name(":program").dotAst.l
    }
  }

  "CPG for code with object's property being accessed" should {
    val cpg = code("""
        |def some_method(param)
        |      if obj.param == some_value
        |        return "Inside is"
        |      end
        |end
        |
        |""".stripMargin)

    "successfully plot ASTs" in {
      cpg.method.name(":program").dotAst.l
    }
  }

  "CPG for code with class having a scoped constant reference" should {
    val cpg = code("""
        |class ModuleName::ClassName
        |  def some_method
        |    puts "Inside the method"
        |  end
        |end
        |""".stripMargin)

    "recognise all literal nodes" in {
      cpg.literal
        .code("\"Inside the method\"")
        .l
        .size shouldBe 1
    }

    "recognise all method nodes" in {
      cpg.method
        .name("some_method")
        .l
        .size shouldBe 1
    }

    "successfully plot ASTs" in {
      cpg.method.name(":program").dotAst.l
    }
  }

  "CPG for code with block splatting expression associations" should {
    val cpg = code("""
        |object.require(:appointment).permit(:param1, :param2, :param3, :param4, :param4,
        |        another_object: [:id, :description, :record_id, :_some_method])
        |""".stripMargin)

    "successfully plot ASTs" in {
      cpg.method.name(":program").dotAst.l
    }
  }

  "CPG for code with alias" should {
    val cpg = code("""
        |def some_method(arg)
        |puts arg
        |end
        |alias :alias_name :some_method
        |alias_name("some param")
        |""".stripMargin)

    "recognise all call nodes" in {
      cpg.call
        .name("puts")
        .l
        .size shouldBe 1
      cpg.call
        .name("some_method")
        .l
        .size shouldBe 1
    }
    "successfully plot ASTs" in {
      cpg.method.name("some_method").dotAst.l
      cpg.method.name(":program").dotAst.l
    }
  }

  "CPG for code with private/protected/public" should {
    val cpg = code("""
        |class SomeClass
        |  private
        |  def method1
        |  end
        |
        |  protected
        |  def method2
        |  end
        |
        |  public
        |  def method3
        |  end
        |end
        |
        |""".stripMargin)

    "recognise all method nodes" in {
      cpg.method
        .name("method1")
        .l
        .size shouldBe 1
      cpg.method
        .name("method1")
        .l
        .size shouldBe 1
      cpg.method
        .name("method3")
        .l
        .size shouldBe 1

    }
    "successfully plot ASTs" in {
      cpg.method.name(":program").dotAst.l
      cpg.method.name("method1").dotAst.l
      cpg.method.name("method2").dotAst.l
      cpg.method.name("method3").dotAst.l
    }
  }
}
