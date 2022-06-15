package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language._

class TopLevelPropertiesTests extends KotlinCode2CpgFixture(withOssDataflow = false) {
  "CPG for code with top-level property" should {
    lazy val cpg = code("""
      |package mypkg
      |
      |const val CSRF_SESSION_KEY = "_csrf"
      |
      |fun main() {
      |  println("Hello, world!")
      |}
      |""".stripMargin)

    // TODO: lower them correctly
    "should not contain LOCAL nodes for the pkg-level consts" in {
      cpg.local.size shouldBe 0
    }
  }
}
