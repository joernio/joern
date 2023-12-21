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

}
