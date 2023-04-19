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


    "local nodes present" in {
      cpg.identifier.name("a").l.size shouldBe 2
      cpg.identifier.name("b").l.size shouldBe 2
      cpg.identifier.name("c").l.size shouldBe 1
    }

    "have a call node for the printHello call" in {
      cpg.call.nameExact("printHello").l match {
        case call :: Nil =>
          call.lineNumber shouldBe Some(7)

        case result => fail(s"Expected printHello call got $result")
      }
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


    "have the correct namespace set" in {
      cpg.namespaceBlock.fullName(".*printhello.php.*").l match {
        case namespaceBlock :: Nil =>
          namespaceBlock.name shouldBe "<global>"
        case result => fail(s"expected namespaceBlock found $result")
      }
    }

    "have a call node for the printHello call" in {
      cpg.call.nameExact("printHello").l match {
        case call :: Nil =>
          call.lineNumber shouldBe Some(7)

        case result => fail(s"Expected printHello call got $result")
      }
    }
  }
}
