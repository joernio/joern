package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.shiftleft.codepropertygraph.generated.ModifierTypes
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

  "record variant" should {
    val cpg = code("""
        |enum Color { Named { name: i32 } }
        |fn main() {
        |  let c = Color::Named { name: 1 };
        |}
        |""".stripMargin)

    "have correct fullName" in {
      cpg.typeDecl.nameExact("Named").fullName.l shouldBe List("rust2cpgtest::Color::Named")
    }

    "have correct inheritsFrom" in {
      cpg.typeDecl
        .fullNameExact("rust2cpgtest::Color::Named")
        .inheritsFromTypeFullName
        .l shouldBe List("rust2cpgtest::Color")
    }

    "have correct members" in {
      inside(cpg.typeDecl.nameExact("Named").member.l) { case name :: Nil =>
        name.name shouldBe "name"
        name.code shouldBe "name: i32"
        name.typeFullName shouldBe "i32"
      }
    }

    "have correct constructor" in {
      inside(cpg.typeDecl.nameExact("Named").method.l) { case init :: Nil =>
        init.name shouldBe "<init>"
        init.fullName shouldBe "rust2cpgtest::Color::Named::<init>"
        init.modifier.modifierType.l shouldBe List(ModifierTypes.CONSTRUCTOR)
        init.methodReturn.typeFullName shouldBe "()"
      }
    }

    "have correct constructor parameters" in {
      inside(cpg.typeDecl.nameExact("Named").method.parameter.sortBy(_.index).l) { case self :: name :: Nil =>
        self.name shouldBe "self"
        self.index shouldBe 0
        self.typeFullName shouldBe "rust2cpgtest::Color::Named"
        name.name shouldBe "name"
        name.index shouldBe 1
        name.typeFullName shouldBe "i32"
      }
    }

    "have correct field assignments" in {
      inside(cpg.typeDecl.nameExact("Named").method.body.astChildren.isCall.l) { case assign :: Nil =>
        // TODO: pending change to self.name = name, once we remove `&` to <init> calls.
        assign.code shouldBe "(*self).name = name"
      }
    }
  }
}
