package io.joern.kotlin2cpg.validation

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class UnitTypeMappingTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  "CPG for code with definition of simple fn returning `kotlin.Unit`" should {
    lazy val cpg = code("""
        |package mypkg
        |
        |fun main() {
        |  println("Hello, world")
        |}
        |""".stripMargin)

    "should contain an METHOD node with `void` in its FULL_NAME prop" in {
      val List(m) = cpg.method.name("main").l
      m.fullName shouldBe "mypkg.main:void()"
    }
  }

}
