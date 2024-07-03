package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.testing.MockCpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MemberTests extends AnyWordSpec with Matchers {

  val cpg = MockCpg()
    .withTypeDecl("foo")
    .cpg

  "Member traversals" should {
    "should find two members: `member` and `static_member`" in {
      cpg.member.name.toSetMutable shouldBe Set("amember")
    }

    "filter by modifier" in {
      val member = cpg.member.hasModifier(ModifierTypes.STATIC).name.toSet
      member shouldBe Set("amember")
    }
  }

}
