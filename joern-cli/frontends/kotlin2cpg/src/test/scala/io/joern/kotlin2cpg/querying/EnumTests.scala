package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.TestContext
import io.shiftleft.semanticcpg.language._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class EnumTests extends AnyFreeSpec with Matchers {

  "CPG for code with simple ENUM definition" - {
    lazy val cpg = TestContext.buildCpg("""
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

    "should contain correct number of calls" in {
      cpg.call.size should not be 0
    }

    "should contain type decl for `Direction` enum" in {
      val List(x) = cpg.typeDecl.fullNameExact("mypkg.Direction").l
      x.name shouldBe "Direction"
      x.isExternal shouldBe false
      x.aliasTypeFullName shouldBe None
    }

    "`Direction` enum should contain the correct members" in {
      val members = cpg.typeDecl.fullNameExact("mypkg.Direction").member.l
      members.size shouldBe 4
      members.map(_.name).toSet shouldBe Set("NORTH", "SOUTH", "WEST", "EAST")
    }

    "should contain type decl for `Color` enum" in {
      val List(x) = cpg.typeDecl.fullNameExact("mypkg.Color").l
      x.name shouldBe "Color"
      x.fullName shouldBe "mypkg.Color"
      x.isExternal shouldBe false
      x.aliasTypeFullName shouldBe None
    }

    "`Color` enum should contain the correct members" in {
      val members = cpg.typeDecl.fullNameExact("mypkg.Color").member.l
      members.size shouldBe 4
      members.map(_.name).toSet shouldBe Set("RED", "GREEN", "BLUE", "rgb")
    }
  }
}
