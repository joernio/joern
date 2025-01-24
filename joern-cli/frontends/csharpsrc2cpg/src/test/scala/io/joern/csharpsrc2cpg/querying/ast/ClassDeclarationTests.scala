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

}
