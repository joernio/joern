package io.joern.rubysrc2cpg.passes.ast

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language._
class TypeDeclAstCreationPassTest extends RubyCode2CpgFixture {

  "AST generation for simple classes declarations" should {

    "generate a basic type declaration node for an empty class" in {
      val cpg = code("""
          |class MyClass
          |end
          |""".stripMargin)
      val Some(myClass) = cpg.typeDecl.nameExact("MyClass").headOption: @unchecked
      myClass.name shouldBe "MyClass"
      myClass.fullName shouldBe "Test0.rb::program:MyClass"
    }

  }

}
