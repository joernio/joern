package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.TestContext
import io.shiftleft.semanticcpg.language._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ScopeCaptureTests extends AnyFreeSpec with Matchers {

  "CPG for code with identifier referencing two possible locals" - {
    lazy val cpg = TestContext.buildCpg("""
        |fun foo() {
        |  val x: String = "n"
        |  1.let {
        |    val x: Int = 1
        |    println(x)
        |  }
        |""".stripMargin)

    "should contain a local inside the scope function with two referencing identifiers" in {
      cpg.local
        .nameExact("x")
        .lineNumber(4)
        .referencingIdentifiers
        .size shouldBe 2
    }

    "should contain a local outside the scope function with a single referenced identifier" in {
      cpg.local
        .nameExact("x")
        .lineNumber(2)
        .referencingIdentifiers
        .size shouldBe 1
    }
  }
}
