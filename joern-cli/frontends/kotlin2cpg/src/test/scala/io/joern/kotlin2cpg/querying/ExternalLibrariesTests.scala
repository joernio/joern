package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.semanticcpg.language._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ExternalLibrariesTests extends AnyFreeSpec with Matchers {
  "CPG for code with usage of the `timber` logging library" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |import timber.log.Timber
        |
        |fun main() {
        |  val foo = "foooooo"
        |  val secretKeys = listOf("LUSHQOXDMZNAIKFREPCYBWVGTJ", "ENIGMA")
        |  Timber.d("TODO: remove from prod: %s", secretKeys);
        |}
        |""".stripMargin)

    "should contain a CALL node for `Timber.d.*` with the correct METHOD_FULL_NAME set" in {
      cpg.call.code("Timber.*").methodFullName.l shouldBe List(
        "timber.log.Timber.d:void(java.lang.String,kotlin.Array)"
      )
    }
  }
}
