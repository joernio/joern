package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.semanticcpg.language.*

class ClassDeclarationTests extends CSharpCode2CpgFixture {

  "empty abstract class" should {
    val cpg = code("""
        |abstract class C {}
        |""".stripMargin)

    "have correct modifiers" in {
      cpg.typeDecl.nameExact("C").modifier.modifierType.sorted.l shouldBe List(
        ModifierTypes.ABSTRACT,
        ModifierTypes.INTERNAL
      )
    }
  }

  "class with member of the same type involved in a fieldAccess" should {
    val cpg = code("""
        |namespace Foo;
        |class Bar
        |{
        | Bar Field;
        | void DoStuff()
        | {
        |   var x = this.Field;
        | }
        |}
        |""".stripMargin)

    "have correct typeDecl properties" in {
      cpg.typeDecl.nameExact("Bar").fullName.l shouldBe List("Foo.Bar")
    }

    "have correct member properties" in {
      cpg.typeDecl.nameExact("Bar").member.nameExact("Field").typeFullName.l shouldBe List("Foo.Bar")
    }
  }

}
