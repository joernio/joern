package io.joern.rubysrc2cpg.deprecated.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class FieldAccessTests extends RubyCode2CpgFixture(useDeprecatedFrontend = true) {

  "Test class field access" should {
    val cpg = code("""
        |class Person
        |  attr_reader :name, :age
        |
        |  def initialize(name, age)
        |    @name = name
        |    @age = age
        |  end
        |end
        |
        |p = Person.new("name", 66)
        |p.age
        |""".stripMargin)

    "be correct for field access" ignore {
      cpg.call.name("age").size shouldBe 1
      val List(program)      = cpg.method.nameExact(":program").l
      val List(programBlock) = program.astChildren.isBlock.l
      val List(call)         = programBlock.astChildren.isCall.codeExact("p.age").l
      call.astChildren.isFieldIdentifier.canonicalNameExact("age").size shouldBe 1
      call.astChildren.isIdentifier.nameExact("p").size shouldBe 1
    }
  }

  "Test array access" should {
    val cpg = code("result = persons[1].age")

    "be correct for filed access" ignore {
      cpg.call.name(":program").l
      val List(program)      = cpg.method.nameExact(":program").l
      val List(programBlock) = program.astChildren.isBlock.l
      val List(call)         = programBlock.astChildren.isCall.l
      val List(rowsCall)     = call.astChildren.isCall.l
      rowsCall.astChildren.isFieldIdentifier.canonicalNameExact("age").size shouldBe 1

      val List(rowsCallLeft) = rowsCall.astChildren.isCall.l
      rowsCallLeft.astChildren.isLiteral.codeExact("1").size shouldBe 1
      rowsCallLeft.astChildren.isIdentifier.nameExact("persons").size shouldBe 1
      call.astChildren.isIdentifier.nameExact("result").size shouldBe 1
    }
  }
}
