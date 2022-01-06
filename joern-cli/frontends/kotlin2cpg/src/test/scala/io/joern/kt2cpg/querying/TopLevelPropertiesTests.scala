package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.semanticcpg.language._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TopLevelPropertiesTests extends AnyFreeSpec with Matchers {
  lazy val cpg = Kt2CpgTestContext.buildCpg("""
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
