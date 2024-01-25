package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.ModifierTypes

class TypeDeclTests extends CSharpCode2CpgFixture {

  "a basic class declaration" should {
    val cpg = code("public class Container {  }", "Container.cs")

    "generate a type declaration with the correct properties" in {
      val x = cpg.typeDecl.nameExact("Container").head
      x.code shouldBe "public class Container"
      x.fullName shouldBe "Container"
      x.filename shouldBe "Container.cs"
      x.aliasTypeFullName shouldBe None
      x.inheritsFromTypeFullName shouldBe Seq.empty
    }

    "generate a type declaration with the correct modifiers" in {
      val x = cpg.typeDecl.nameExact("Container").head
      x.modifier.modifierType.head shouldBe ModifierTypes.PUBLIC
    }
  }

  "a basic class declaration within a namespace" should {
    val cpg = code(
      """namespace SampleNamespace
        |{
        |    private class SampleClass { }
        |}
        |""".stripMargin,
      "SampleClass.cs"
    )

    "generate a type declaration with the correct properties" in {
      val x = cpg.typeDecl.nameExact("SampleClass").head
      x.code shouldBe "private class SampleClass"
      x.fullName shouldBe "SampleNamespace.SampleClass"
      x.filename shouldBe "SampleClass.cs"
      x.aliasTypeFullName shouldBe None
      x.inheritsFromTypeFullName shouldBe Seq.empty
    }

    "generate a type declaration with the correct modifiers" in {
      val x = cpg.typeDecl.nameExact("SampleClass").head
      x.modifier.modifierType.head shouldBe ModifierTypes.PRIVATE
    }
  }

  "a basic struct declaration" should {
    val cpg = code("""
        |public struct Coords
        |{
        |    public double y;
        |}
        |""".stripMargin)

    "generate a type declaration with correct properties" in {
      inside(cpg.typeDecl.nameExact("Coords").headOption) {
        case Some(struct) => struct.fullName shouldBe "Coords"
        case None         => fail("Unable to find `Coords` type decl node")

      }
    }

    "generate a type declaration with correct modifiers" in {
      inside(cpg.typeDecl.nameExact("Coords").headOption) {
        case Some(struct) => struct.modifier.modifierType.head shouldBe ModifierTypes.PUBLIC
        case None         => fail("Unable to find modifier for `Coords` type decl node")
      }
    }

    "generate a type declaration with correct member" in {
      inside(cpg.typeDecl.nameExact("Coords").headOption) {
        case Some(struct) => struct.member.name.head shouldBe "y"
        case None         => fail("Unable to find member for `Coords` type decl node")
      }
    }
  }
}
