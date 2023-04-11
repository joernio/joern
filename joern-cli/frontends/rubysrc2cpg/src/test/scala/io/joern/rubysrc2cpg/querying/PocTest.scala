package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language._

class PocTest extends RubyCode2CpgFixture {

  "The CPG generated for a very simple example" should {
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
