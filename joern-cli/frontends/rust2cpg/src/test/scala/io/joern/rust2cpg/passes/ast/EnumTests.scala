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

    "have correct field access" in {
      inside(cpg.assignment.where(_.target.isIdentifier.nameExact("c")).source.l) { case (fieldAccess: Call) :: Nil =>
        fieldAccess.methodFullName shouldBe Operators.fieldAccess
        fieldAccess.code shouldBe "Color::Red"
        fieldAccess.typeFullName shouldBe "rust2cpgtest::Color"
        inside(fieldAccess.argument.sortBy(_.argumentIndex).l) {
          case (base: TypeRef) :: (field: FieldIdentifier) :: Nil =>
            base.code shouldBe "Color"
            base.typeFullName shouldBe "rust2cpgtest::Color"
            field.canonicalName shouldBe "Red"
        }
      }
    }
  }

  "module-qualified unit variant" should {
    val cpg = code("""
        |mod m {
        |  pub enum Color { Red }
        |}
        |fn main() {
        |  let c = m::Color::Red;
        |}
        |""".stripMargin)

    "have correct fullName" in {
      cpg.typeDecl.nameExact("Color").fullName.l shouldBe List("rust2cpgtest::m::Color")
    }

    "have correct members" in {
      inside(cpg.typeDecl.nameExact("Color").member.l) { case red :: Nil =>
        red.name shouldBe "Red"
        red.code shouldBe "Red"
        red.typeFullName shouldBe "rust2cpgtest::m::Color"
      }
    }

    "have correct field access" in {
      inside(cpg.assignment.where(_.target.isIdentifier.nameExact("c")).source.l) { case (fieldAccess: Call) :: Nil =>
        fieldAccess.name shouldBe Operators.fieldAccess
        fieldAccess.code shouldBe "m::Color::Red"
        fieldAccess.typeFullName shouldBe "rust2cpgtest::m::Color"
        inside(fieldAccess.argument.sortBy(_.argumentIndex).l) {
          case (base: TypeRef) :: (field: FieldIdentifier) :: Nil =>
            base.code shouldBe "m::Color"
            base.typeFullName shouldBe "rust2cpgtest::m::Color"
            field.canonicalName shouldBe "Red"
        }
      }
    }
  }

  "self-qualified unit variant" should {
    val cpg = code("""
        |enum Color { Red }
        |impl Color {
        |  fn red() -> Color { Self::Red }
        |}
        |""".stripMargin)

    "have correct fullName" in {
      cpg.typeDecl.nameExact("Color").fullName.l shouldBe List("rust2cpgtest::Color")
    }

    "have correct members" in {
      inside(cpg.typeDecl.nameExact("Color").member.l) { case red :: Nil =>
        red.name shouldBe "Red"
        red.code shouldBe "Red"
        red.typeFullName shouldBe "rust2cpgtest::Color"
      }
    }

    "have correct field access" in {
      inside(cpg.method.nameExact("red").ast.isCall.nameExact(Operators.fieldAccess).l) { case fieldAccess :: Nil =>
        fieldAccess.code shouldBe "Self::Red"
        fieldAccess.typeFullName shouldBe "rust2cpgtest::Color"
        inside(fieldAccess.argument.sortBy(_.argumentIndex).l) {
          case (base: TypeRef) :: (field: FieldIdentifier) :: Nil =>
            base.code shouldBe "Self"
            base.typeFullName shouldBe "rust2cpgtest::Color"
            field.canonicalName shouldBe "Red"
        }
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

  "record variant pattern in an if-let" should {
    val cpg = code("""
        |enum E { A { x: i32 }, B }
        |fn foo(e: E) {
        |  if let E::A { x } = e {
        |    bar(x);
        |  }
        |}
        |""".stripMargin)

    "have correct field access" in {
      inside(cpg.assignment.where(_.target.isIdentifier.nameExact("x")).source.l) { case (fieldAccess: Call) :: Nil =>
        fieldAccess.methodFullName shouldBe Operators.fieldAccess
        fieldAccess.code shouldBe "(<tmp>0 as rust2cpgtest::E::A).x"
        fieldAccess.typeFullName shouldBe "i32"

        inside(fieldAccess.argument.sortBy(_.argumentIndex).l) { case (cast: Call) :: (field: FieldIdentifier) :: Nil =>
          field.canonicalName shouldBe "x"

          cast.methodFullName shouldBe Operators.cast
          cast.code shouldBe "(<tmp>0 as rust2cpgtest::E::A)"
          cast.typeFullName shouldBe "rust2cpgtest::E::A"

          inside(cast.argument.sortBy(_.argumentIndex).l) { case (typeRef: TypeRef) :: (tmp: Identifier) :: Nil =>
            typeRef.code shouldBe "E::A"
            typeRef.typeFullName shouldBe "rust2cpgtest::E::A"

            tmp.name shouldBe "<tmp>0"
            tmp.typeFullName shouldBe "rust2cpgtest::E"
          }
        }
      }
    }
  }

  "generic tuple variant pattern in a match arm" should {
    val cpg = code("""
        |enum E<T> { A(T, bool) }
        |fn foo(e: E<i32>) {
        |  match e {
        |  E::A(x, y) => bar(x),
        |  _ => 0,
        |  };
        |}
        |""".stripMargin)

    "have correct field accesses" in {
      inside(cpg.assignment.where(_.target.isIdentifier.nameExact("x")).source.l) { case (fieldAccess: Call) :: Nil =>
        fieldAccess.methodFullName shouldBe Operators.fieldAccess
        fieldAccess.code shouldBe "(<tmp>0 as rust2cpgtest::E<T>::A).0"
        fieldAccess.typeFullName shouldBe "i32"

        inside(fieldAccess.argument.sortBy(_.argumentIndex).l) { case (cast: Call) :: (field: FieldIdentifier) :: Nil =>
          field.canonicalName shouldBe "0"

          cast.methodFullName shouldBe Operators.cast
          cast.code shouldBe "(<tmp>0 as rust2cpgtest::E<T>::A)"
          cast.typeFullName shouldBe "rust2cpgtest::E<T>::A"

          inside(cast.argument.sortBy(_.argumentIndex).l) { case (typeRef: TypeRef) :: (tmp: Identifier) :: Nil =>
            typeRef.code shouldBe "E::A"
            typeRef.typeFullName shouldBe "rust2cpgtest::E<T>::A"

            tmp.name shouldBe "<tmp>0"
            tmp.typeFullName shouldBe "rust2cpgtest::E<i32>"
          }
        }
      }

      inside(cpg.assignment.where(_.target.isIdentifier.nameExact("y")).source.l) { case (fieldAccess: Call) :: Nil =>
        fieldAccess.methodFullName shouldBe Operators.fieldAccess
        fieldAccess.code shouldBe "(<tmp>0 as rust2cpgtest::E<T>::A).1"
        fieldAccess.typeFullName shouldBe "bool"

        inside(fieldAccess.argument.sortBy(_.argumentIndex).l) { case (cast: Call) :: (field: FieldIdentifier) :: Nil =>
          field.canonicalName shouldBe "1"

          cast.methodFullName shouldBe Operators.cast
          cast.code shouldBe "(<tmp>0 as rust2cpgtest::E<T>::A)"
          cast.typeFullName shouldBe "rust2cpgtest::E<T>::A"

          inside(cast.argument.sortBy(_.argumentIndex).l) { case (typeRef: TypeRef) :: (tmp: Identifier) :: Nil =>
            typeRef.code shouldBe "E::A"
            typeRef.typeFullName shouldBe "rust2cpgtest::E<T>::A"

            tmp.name shouldBe "<tmp>0"
            tmp.typeFullName shouldBe "rust2cpgtest::E<i32>"
          }
        }
      }
    }
  }

  "nested tuple variant patterns in a let" should {
    val cpg = code("""
        |enum E { A(i32) }
        |enum F { B(E) }
        |fn foo(value: F) {
        |  let F::B(E::A(x)) = value;
        |}
        |""".stripMargin)

    "have correct field accesses" in {
      inside(cpg.assignment.where(_.target.isIdentifier.nameExact("x")).source.l) { case (fieldAccessA: Call) :: Nil =>
        fieldAccessA.methodFullName shouldBe Operators.fieldAccess
        fieldAccessA.code shouldBe "((value as rust2cpgtest::F::B).0 as rust2cpgtest::E::A).0"
        fieldAccessA.typeFullName shouldBe "i32"

        inside(fieldAccessA.argument.sortBy(_.argumentIndex).l) {
          case (castA: Call) :: (fieldA: FieldIdentifier) :: Nil =>
            fieldA.canonicalName shouldBe "0"

            castA.methodFullName shouldBe Operators.cast
            castA.code shouldBe "((value as rust2cpgtest::F::B).0 as rust2cpgtest::E::A)"
            castA.typeFullName shouldBe "rust2cpgtest::E::A"

            inside(castA.argument.sortBy(_.argumentIndex).l) {
              case (typeRefA: TypeRef) :: (fieldAccessB: Call) :: Nil =>
                typeRefA.code shouldBe "E::A"
                typeRefA.typeFullName shouldBe "rust2cpgtest::E::A"

                fieldAccessB.methodFullName shouldBe Operators.fieldAccess
                fieldAccessB.code shouldBe "(value as rust2cpgtest::F::B).0"
                fieldAccessB.typeFullName shouldBe "rust2cpgtest::E"

                inside(fieldAccessB.argument.sortBy(_.argumentIndex).l) {
                  case (castB: Call) :: (fieldB: FieldIdentifier) :: Nil =>
                    fieldB.canonicalName shouldBe "0"

                    castB.methodFullName shouldBe Operators.cast
                    castB.code shouldBe "(value as rust2cpgtest::F::B)"
                    castB.typeFullName shouldBe "rust2cpgtest::F::B"

                    inside(castB.argument.sortBy(_.argumentIndex).l) {
                      case (typeRefB: TypeRef) :: (value: Identifier) :: Nil =>
                        typeRefB.code shouldBe "F::B"
                        typeRefB.typeFullName shouldBe "rust2cpgtest::F::B"

                        value.name shouldBe "value"
                        value.typeFullName shouldBe "rust2cpgtest::F"
                    }
                }
            }
        }
      }
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
