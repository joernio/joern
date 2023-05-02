package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language._

class IdentifierTests extends RubyCode2CpgFixture {

  "The CPG generated for a multiplication example" should {
    val cpg = code(
      """
        |# call instance methods
        |a = 1
        |b = 2
        |a = 3
        |b = 4
        |c = a*b
        |puts "Multiplication is : #{c}"
        |""".stripMargin,
      fileName = "multiply.rb"
    )

    "identifier nodes present" in {
      cpg.identifier.name("a").l.size shouldBe 3
      cpg.identifier.name("b").l.size shouldBe 3
      cpg.identifier.name("c").l.size shouldBe 2
    }

    "literal nodes present" in {
      cpg.literal.code("1").l.size shouldBe 1
      cpg.literal.code("2").l.size shouldBe 1
      cpg.literal.code("3").l.size shouldBe 1
      cpg.literal.code("4").l.size shouldBe 1
    }
  }

  "The CPG generated for a class" should {
    val cpg = code(
      """
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
        |Person p
        |p.greet()
        |""".stripMargin,
      fileName = "classtest.rb"
    )

    "identifier nodes present" in {
      cpg.identifier.name("name").l.size shouldBe 2
      cpg.identifier.name("age").l.size shouldBe 2
      cpg.identifier.name("@name").l.size shouldBe 2
      cpg.identifier.name("@age").l.size shouldBe 4
      cpg.identifier.size shouldBe 12
    }
  }

  "The CPG generated for a multiplication example" should {
    val cpg = code(
      """
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
        |""".stripMargin,
      fileName = "sum.rb"
    )

    "function test" in {
      cpg.identifier.name("a").l.size shouldBe 1
      cpg.identifier.name("b").l.size shouldBe 1
      cpg.identifier.name("c").l.size shouldBe 1
      cpg.identifier.name("sumOfThree").l.size shouldBe 1
      cpg.identifier.name("num1").l.size shouldBe 1
      cpg.identifier.name("num2").l.size shouldBe 2
      cpg.identifier.name("num3").l.size shouldBe 2
      cpg.identifier.name("sum").l.size shouldBe 2
      val lst = cpg.identifier.l
      lst.size shouldBe 11
    }

    "The CPG generated for a expressions" should {
      val cpg = code(
        """
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
          |""".stripMargin,
        fileName = "sum.rb"
      )

      "expression test" in {
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

    "The CPG generated for begin and end blocks" should {
      val cpg = code(
        """
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
          |""".stripMargin,
        fileName = "beginning_of_the_end.rb"
      )

      "beginning_of_the_end test" in {
        cpg.identifier.name("beginvar").l.size shouldBe 2
        cpg.identifier.name("endvar").l.size shouldBe 2
        cpg.identifier.name("beginbool").l.size shouldBe 1
        cpg.identifier.name("endbool").l.size shouldBe 1
        cpg.identifier.size shouldBe 6
      }
    }

    "The CPG generated for do block" should {
      val cpg = code(
        """
          |[1, 2, "three"].each do |n|
          | puts n
          |end
          |""".stripMargin,
        fileName = "doblock.rb"
      )

      "doblocktest" in {
        cpg.identifier.name("n").l.size shouldBe 2
        cpg.identifier.size shouldBe 2
      }
    }

    "The CPG generated for failing test" should {
      val cpg = code(
        """
          |[1, 2, "three"].each do |n, m|
          |  expect {
          |  someObject.someMethod(n)
          |  someObject.someMethod(m)
          |  }.to otherMethod(n).by(1)
          |end
          |
          |""".stripMargin,
        fileName = "dofailing.rb"
      )

      "dofailing" in {
        cpg.identifier.name("n").l.size shouldBe 1
        cpg.identifier.name("m").l.size shouldBe 1
        cpg.identifier.size shouldBe 2
      }
    }

    "The CPG generated for rails test" should {
      val cpg = code(
        """
          |Rails.application.configure do
          |  config.log_formatter = ::Logger::Formatter.new
          |end
          |
          |""".stripMargin,
        fileName = "dofailing.rb"
      )

      "railstest" in {
        cpg.identifier.size shouldBe 5
      }
    }
  }
}
