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
      cpg.identifier.name("name").l.size shouldBe 2
      cpg.identifier.name("age").l.size shouldBe 2
      cpg.identifier.name("@name").l.size shouldBe 2
      cpg.identifier.name("@age").l.size shouldBe 4
      cpg.call.name("attr_accessor").size shouldBe 1
      cpg.call.name("greet").size shouldBe 1
      cpg.method.name("initialize").size shouldBe 1
      cpg.method.name("greet").size shouldBe 1
      cpg.call.name("puts").size shouldBe 2
      cpg.method.name("have_birthday").size shouldBe 1
      cpg.identifier.size shouldBe 13
    }
  }

  "CPG for code with a function call and arguments" should {
    val cpg = code("""
        |def add_three_numbers(num1, num2, num3)
        |  sum = num1 + num2 + num3
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
      cpg.identifier.name("num2").l.size shouldBe 2
      cpg.identifier.name("num3").l.size shouldBe 2
      cpg.identifier.name("sum").l.size shouldBe 2
      cpg.call.name("add_three_numbers").size shouldBe 1
      cpg.call.name("num1").size shouldBe 1
      cpg.identifier.size shouldBe 14
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
        cpg.identifier.name("p").l.size shouldBe 1  // conditionalOperatorExpression
        cpg.identifier.size shouldBe 44
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
    }

    "CPG for code with square brackets as methods" should {
      val cpg = code("""
          |class MyClass
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
        cpg.method.name("\\[]").size shouldBe 1
        cpg.method.name("\\[]=").size shouldBe 1
        cpg.call.name("=").size shouldBe 3
        cpg.method.name("initialize").size shouldBe 1
        cpg.call.name("to_s").size shouldBe 1
        cpg.call.name("new").size shouldBe 1
        cpg.call.size shouldBe 5
        cpg.identifier.name("@my_hash").size shouldBe 3
        cpg.identifier.name("key").size shouldBe 3
        cpg.identifier.name("value").size shouldBe 2
        cpg.identifier.name("my_object").size shouldBe 1
        /*
         * FIXME
         *  def []=(key, value) gets parsed incorrectly with parser error "no viable alternative at input 'def []=(key, value)'"
         *  This needs a fix in the parser and update to this UT after the fix
         * FIXME
         *  MyClass is identified as a variableIdentifier and so an identifier. This needs to be fixed
         */
      }
    }
  }
}
