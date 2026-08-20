package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.shiftleft.codepropertygraph.generated.{ModifierTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
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

  "tuple variant" should {
    val cpg = code("""
        |enum Color { Rgb(i32, bool) }
        |fn main() {
        |  let c = Color::Rgb(1, true);
        |}
        |""".stripMargin)

    "have correct fullName" in {
      cpg.typeDecl.nameExact("Rgb").fullName.l shouldBe List("rust2cpgtest::Color::Rgb")
    }

    "have correct inheritsFrom" in {
      cpg.typeDecl
        .fullNameExact("rust2cpgtest::Color::Rgb")
        .inheritsFromTypeFullName
        .l shouldBe List("rust2cpgtest::Color")
    }

    "have correct members" in {
      inside(cpg.typeDecl.nameExact("Rgb").member.l) { case zero :: one :: Nil =>
        zero.name shouldBe "0"
        zero.code shouldBe "i32"
        zero.typeFullName shouldBe "i32"

        one.name shouldBe "1"
        one.code shouldBe "bool"
        one.typeFullName shouldBe "bool"
      }
    }

    "have correct constructor" in {
      inside(cpg.typeDecl.nameExact("Rgb").method.l) { case init :: Nil =>
        init.name shouldBe "<init>"
        init.fullName shouldBe "rust2cpgtest::Color::Rgb::<init>"
        init.modifier.modifierType.l shouldBe List(ModifierTypes.CONSTRUCTOR)
        init.methodReturn.typeFullName shouldBe "()"
      }
    }

    "have correct constructor parameters" in {
      inside(cpg.typeDecl.nameExact("Rgb").method.parameter.sortBy(_.index).l) { case self :: zero :: one :: Nil =>
        self.name shouldBe "self"
        self.index shouldBe 0
        self.typeFullName shouldBe "rust2cpgtest::Color::Rgb"

        zero.name shouldBe "0"
        zero.index shouldBe 1
        zero.typeFullName shouldBe "i32"

        one.name shouldBe "1"
        one.index shouldBe 2
        one.typeFullName shouldBe "bool"
      }
    }

    "have correct field assignments" in {
      inside(cpg.typeDecl.nameExact("Rgb").method.body.astChildren.assignment.l) {
        case assignZero :: assignOne :: Nil =>
          // TODO: pending change to self.0 = 0, once we remove `&` to <init> calls.
          assignZero.code shouldBe "(*self).0 = 0"
          assignOne.code shouldBe "(*self).1 = 1"
      }
    }

    "have correct ctor wrapper" in {
      inside(cpg.method.nameExact("Rgb").l) { case ctor :: Nil =>
        ctor.fullName shouldBe "rust2cpgtest::Color::Rgb"
        ctor.modifier shouldBe empty
        ctor.methodReturn.typeFullName shouldBe "rust2cpgtest::Color"
      }
    }

    "have correct ctor wrapper parameters" in {
      inside(cpg.method.nameExact("Rgb").parameter.sortBy(_.index).l) { case zero :: one :: Nil =>
        zero.name shouldBe "0"
        zero.index shouldBe 1
        zero.typeFullName shouldBe "i32"

        one.name shouldBe "1"
        one.index shouldBe 2
        one.typeFullName shouldBe "bool"
      }
    }

    "have correct ctor wrapper body" in {
      inside(cpg.method.nameExact("Rgb").body.astChildren.isCall.l) { case allocAssign :: initCall :: Nil =>
        allocAssign.code shouldBe s"<tmp>0 = ${Operators.alloc}"
        initCall.name shouldBe "<init>"
        initCall.methodFullName shouldBe "rust2cpgtest::Color::Rgb::<init>"

        // TODO: pending change to <init>(tmp0, 0, 1), once we remove `&` to <init> calls.
        initCall.code shouldBe "Rgb::<init>(&<tmp>0, 0, 1)"
        inside(initCall.argument.sortBy(_.argumentIndex).l) {
          case (addressOf: Call) :: (zero: Identifier) :: (one: Identifier) :: Nil =>
            addressOf.code shouldBe "&<tmp>0"
            addressOf.argumentIndex shouldBe 0
            addressOf.typeFullName shouldBe "&rust2cpgtest::Color::Rgb"

            zero.name shouldBe "0"
            zero.typeFullName shouldBe "i32"

            one.name shouldBe "1"
            one.typeFullName shouldBe "bool"
        }
      }
    }
  }

  "generic tuple enum" should {
    val cpg = code("""
        |enum Wrapper<T> { One(T) }
        |fn main() {
        |  let w = Wrapper::One(5u8);
        |}
        |""".stripMargin)

    "have correct variant fullName" in {
      cpg.typeDecl.nameExact("One").fullName.l shouldBe List("rust2cpgtest::Wrapper<T>::One")
    }

    "have correct inheritsFrom" in {
      cpg.typeDecl
        .fullNameExact("rust2cpgtest::Wrapper<T>::One")
        .inheritsFromTypeFullName
        .l shouldBe List("rust2cpgtest::Wrapper<T>")
    }

    "have correct ctor wrapper fullName" in {
      cpg.method.nameExact("One").fullName.l shouldBe List("rust2cpgtest::Wrapper<T>::One")
    }

    "have the same fullName as the one at the call site" in {
      cpg.call.nameExact("One").methodFullName.l shouldBe List("rust2cpgtest::Wrapper<T>::One")
    }
  }

  "same-named enums in both branches of an if" should {
    val cpg = code("""
        |fn outer(c: bool) {
        |  if c {
        |    enum Inner { Red(i32) }
        |    Inner::Red(1);
        |  } else {
        |    enum Inner { Green(bool) }
        |    Inner::Green(true);
        |  }
        |}
        |""".stripMargin)

    "have correct fullNames" in {
      inside(cpg.typeDecl.nameExact("Inner").sortBy(_.lineNumber).l) { case thenInner :: elseInner :: Nil =>
        thenInner.fullName shouldBe "rust2cpgtest::outer::Inner#1"
        elseInner.fullName shouldBe "rust2cpgtest::outer::Inner#2"
      }
    }

    "have correct variant fullNames" in {
      cpg.typeDecl.nameExact("Red").fullName.l shouldBe List("rust2cpgtest::outer::Inner#1::Red")
      cpg.typeDecl.nameExact("Green").fullName.l shouldBe List("rust2cpgtest::outer::Inner#2::Green")
    }

    "have correct ctor wrapper fullNames" in {
      cpg.method.nameExact("Red").fullName.l shouldBe List("rust2cpgtest::outer::Inner#1::Red")
      cpg.method.nameExact("Green").fullName.l shouldBe List("rust2cpgtest::outer::Inner#2::Green")
    }
  }
}
