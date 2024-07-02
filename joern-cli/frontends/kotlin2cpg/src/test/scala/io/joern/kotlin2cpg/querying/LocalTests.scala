package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class LocalTests extends KotlinCode2CpgFixture(withOssDataflow = false) {
  "CPG for code simple local declarations" should {
    val cpg = code("""
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
      l1.typeFullName shouldBe "int"

      val List(l2) = cpg.local("y").l
      l2.code shouldBe "y"
      l2.name shouldBe "y"
      l2.typeFullName shouldBe "int"
    }
  }
}
