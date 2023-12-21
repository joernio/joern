package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.semanticcpg.language.*

class MethodTests extends CSharpCode2CpgFixture {

  "a basic class declaration with method" should {
    val cpg = code(basicBoilerplate(), "Program.cs")

    "generate a method node with type decl parent" in {
      val x = cpg.method.nameExact("Main").head
      x.fullName should startWith("HelloWorld.Program.Main:void")
      x.fullName shouldBe "HelloWorld.Program.Main:void(System.String[])"
      x.signature shouldBe "void(System.String[])"
      x.filename shouldBe "Program.cs"
      x.code shouldBe "static void Main(string[] args)"

      x.typeDecl match
        case Some(typeDecl) => typeDecl.name shouldBe "Program"
        case None           => fail("No TYPE_DECL parent found!")
    }

    "generate a method node with the correct modifiers" in {
      val List(x, y) = cpg.method.nameExact("Main").modifier.l: @unchecked
      x.modifierType shouldBe ModifierTypes.INTERNAL
      y.modifierType shouldBe ModifierTypes.STATIC
    }

    "generate a method node with a parameter" in {
      val List(x) = cpg.method.nameExact("Main").parameter.l: @unchecked
      x.name shouldBe "args"
    }

    "generate a method node with a block" in {
      cpg.method.nameExact("Main").body.l should not be empty
    }
  }

}
