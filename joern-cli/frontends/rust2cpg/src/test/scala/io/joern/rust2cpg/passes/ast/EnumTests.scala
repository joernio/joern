package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.shiftleft.semanticcpg.language.*

class EnumTests extends Rust2CpgSuite(noSysRoot = true) {

  "unit variant" should {
    val cpg = code("""
        |enum Color { Red, Green }
        |fn main() {
        |  let c = Color::Red;
        |}
        |""".stripMargin)

    "have correct fullName" in {
      cpg.typeDecl.nameExact("Color").fullName.l shouldBe List("rust2cpgtest::Color")
    }

    "have correct members" in {
      inside(cpg.typeDecl.nameExact("Color").member.l) { case red :: green :: Nil =>
        red.name shouldBe "Red"
        red.code shouldBe "Red"
        red.typeFullName shouldBe "rust2cpgtest::Color"

        green.name shouldBe "Green"
        green.code shouldBe "Green"
        green.typeFullName shouldBe "rust2cpgtest::Color"
      }
    }
  }
}
