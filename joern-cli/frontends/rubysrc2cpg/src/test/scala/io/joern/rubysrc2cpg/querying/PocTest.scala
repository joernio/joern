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
        |# define a class
        |class Box
        |   BOX_COMPANY = "TATA Inc"
        |   BOXWEIGHT = 10
        |   # constructor method
        |   def initialize(w,h)
        |      @width, @height = w, h
        |   end
        |   # instance method
        |   def getArea
        |      @width * @height
        |   end
        |end
        |
        |# create an object
        |box = Box.new(10, 20)
        |
        |# call instance methods
        |a = box.getArea()
        |puts "Area of the box is : #{a}"
        |puts Box::BOX_COMPANY
        |puts "Box weight is: #{Box::BOXWEIGHT}"
        |""".stripMargin,
      fileName = "printhello.rb"
    )


    "identifier nodes present" in {
      cpg.identifier.name("box").l.size shouldBe 1
      cpg.identifier.name("a").l.size shouldBe 1
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
      lst.size shouldBe 7
    }
  }
}
