package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.TestContext
import io.shiftleft.codepropertygraph.generated.nodes.Local
import io.shiftleft.semanticcpg.language._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class LocalTests extends AnyFreeSpec with Matchers {
  "CPG for code simple local declarations" - {
    lazy val cpg = TestContext.buildCpg("""
        |fun main() {
        |  val x: Int = 1
        |  val y = 2
        |  println(x + y)
        |}
        |""".stripMargin)

    "should contain LOCAL node for `x` and `y` with correct props set" in {
      val List(l1) = cpg.local("x").l
      l1.code shouldBe "x"
      l1.name shouldBe "x"
      l1.typeFullName shouldBe "java.lang.Integer"
      l1.order shouldBe 1

      val List(l2) = cpg.local("y").l
      l2.code shouldBe "y"
      l2.name shouldBe "y"
      l2.typeFullName shouldBe "java.lang.Integer"
      l2.order shouldBe 3
    }
  }
}
