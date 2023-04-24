package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language._

class PocTest extends RubyCode2CpgFixture {

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
      cpg.identifier.name("a").l.size shouldBe 2
      cpg.identifier.name("b").l.size shouldBe 2
      cpg.identifier.name("c").l.size shouldBe 1
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
        |""".stripMargin,
      fileName = "classtest.rb"
    )


    "identifier nodes present" in {
      cpg.identifier.name("name").l.size shouldBe 2
      cpg.identifier.name("age").l.size shouldBe 2
      cpg.identifier.name("@name").l.size shouldBe 1
      cpg.identifier.name("@age").l.size shouldBe 2
      cpg.identifier.size shouldBe 7
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
      cpg.identifier.name("num2").l.size shouldBe 1
      cpg.identifier.name("num3").l.size shouldBe 1
      val lst = cpg.identifier.l
      lst.size shouldBe 8
    }

    "The CPG generated for a modifiers and expressions" should {
      val cpg = code(
        """
          |a = 1
          |b = 2 if a > 1
          |""".stripMargin,
        fileName = "sum.rb"
      )

      "function test" in {
        cpg.identifier.name("a").l.size shouldBe 2
        cpg.identifier.name("b").l.size shouldBe 1
        cpg.identifier.size shouldBe 3
      }
    }
  }
}
