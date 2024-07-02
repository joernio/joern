package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class EnumTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  "CPG for code with simple enum declaration" should {
    val cpg = code("""
        |package mypkg
        |
        |enum class Direction {
        |    NORTH, SOUTH, WEST, EAST
        |}
        |
        |enum class Color(val rgb: Int) {
        |    RED(0xFF0000),
        |    GREEN(0x00FF00),
        |    BLUE(0x0000FF)
        |}
        |
        |fun main(args : Array<String>) {
        |  println(Direction.NORTH)
        |  println(Color.RED)
        |}
        |""".stripMargin)

    "should contain a TYPE_DECL node for the `Direction` enum with the correct props set" in {
      val List(td) = cpg.typeDecl.fullNameExact("mypkg.Direction").l
      td.name shouldBe "Direction"
      td.isExternal shouldBe false
      td.aliasTypeFullName shouldBe None

      td.member.size shouldBe 4
      td.member.map(_.name).toSet shouldBe Set("NORTH", "SOUTH", "WEST", "EAST")
    }

    "should contain type decl for `Color` enum with the correct props set" in {
      val List(td) = cpg.typeDecl.fullNameExact("mypkg.Color").l
      td.name shouldBe "Color"
      td.fullName shouldBe "mypkg.Color"
      td.isExternal shouldBe false
      td.aliasTypeFullName shouldBe None

      td.member.size shouldBe 4
      td.member.map(_.name).toSet shouldBe Set("RED", "GREEN", "BLUE", "rgb")
    }
  }
}
