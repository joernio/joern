package io.joern.kotlin2cpg.postProcessing

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.joern.x2cpg.Defines
import io.shiftleft.semanticcpg.language.{ICallResolver, NoResolve}
import io.shiftleft.semanticcpg.language.*

class TypeRecoveryPassTest extends KotlinCode2CpgFixture(withPostProcessing = true) {

  implicit val resolver: ICallResolver = NoResolve

  "Identifier exposed via import" should {
    val cpg = code("""
        |package mypkg
        |import com.firebase.ui.auth.AuthUI
        |
        |fun main() {
        |   val intent = AuthUI.getInstance().createSignInIntentBuilder()
        |                    .setAvailableProviders(signInProviders)
        |                    .setLogo(R.drawable.ic_fire_emoji)
        |                    .build()
        |}
        |""".stripMargin)

    "have type resolved for identifier" in {
      val List(i) = cpg.identifier("AuthUI").l
      i.typeFullName shouldBe "com.firebase.ui.auth.AuthUI"
    }

    "be able to faciliate methodFullName resolution for call made from identifier object" in {
      cpg.call("getInstance").methodFullName.l shouldBe
        List(s"com.firebase.ui.auth.AuthUI.getInstance:${Defines.UnresolvedSignature}(0)")
    }

    "be able to faciliate methodFullName resolution for call chaining" ignore {
      cpg.call("createSignInIntentBuilder").methodFullName.l shouldBe List(
        "com.firebase.ui.auth.AuthUI.getInstance:ANY().<returnValue>.createSignInIntentBuilder"
      )
    }
  }

}
