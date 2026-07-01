package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class OperatorTests extends Rust2CpgSuite(noSysRoot = true) {

  "an `as` expression" should {
    val cpg = code("""
        |fn main(x: i32) {
        | let y = x as i64;
        |}
        |""".stripMargin)

    "lower to a cast call with the target type as typeFullName" in {
      inside(cpg.call.nameExact(Operators.cast).l) { case cast :: Nil =>
        cast.code shouldBe "x as i64"
        cast.methodFullName shouldBe Operators.cast
        cast.typeFullName shouldBe "i64"
      }
    }

    "have a TypeRef to the target type as the first argument" in {
      inside(cpg.call.nameExact(Operators.cast).argument(1).l) { case (typeRef: TypeRef) :: Nil =>
        typeRef.code shouldBe "i64"
        typeRef.typeFullName shouldBe "i64"
      }
    }

    "have the cast operand as the second argument" in {
      inside(cpg.call.nameExact(Operators.cast).argument(2).l) { case (x: Identifier) :: Nil =>
        x.name shouldBe "x"
        x.typeFullName shouldBe "i32"
      }
    }
  }

  "a single index expression" should {
    val cpg = code("""
        |fn foo(xs: Vec<i32>, i: usize) -> i32 {
        | xs[i]
        |}
        |""".stripMargin)

    "lower to an indexAccess call with the element type as typeFullName" in {
      inside(cpg.call.nameExact(Operators.indexAccess).l) { case indexAccess :: Nil =>
        indexAccess.code shouldBe "xs[i]"
        indexAccess.methodFullName shouldBe Operators.indexAccess
        indexAccess.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        indexAccess.typeFullName shouldBe "i32"
      }
    }

    // TODO (rust_ast_gen): confirm why it doesn't have a type
    "have the base as the first argument" in {
      inside(cpg.call.nameExact(Operators.indexAccess).argument(1).l) { case (xs: Identifier) :: Nil =>
        xs.name shouldBe "xs"
        xs.typeFullName shouldBe Defines.Any
      }
    }

    "have the index as the second argument" in {
      inside(cpg.call.nameExact(Operators.indexAccess).argument(2).l) { case (i: Identifier) :: Nil =>
        i.name shouldBe "i"
        i.typeFullName shouldBe "usize"
      }
    }
  }

  "a nested index expression" should {
    val cpg = code("""
        |fn foo(xs: Vec<Vec<i32>>, i: usize, j: usize) -> i32 {
        | xs[i][j]
        |}
        |""".stripMargin)

    "lower to two indexAccess calls" in {
      cpg.call.nameExact(Operators.indexAccess).code.toSet shouldBe Set("xs[i]", "xs[i][j]")
    }

    "have an i32 typeFullName for the outer indexAccess" in {
      cpg.call.nameExact(Operators.indexAccess).codeExact("xs[i][j]").typeFullName.l shouldBe List("i32")
    }

    // TODO (rust_ast_gen): confirm that no type for the inner is to be expected
    "have Any as typeFullName for the inner indexAccess" in {
      cpg.call.nameExact(Operators.indexAccess).codeExact("xs[i]").typeFullName.l shouldBe List(Defines.Any)
    }

    "have the inner indexAccess as the first argument of the outer indexAccess" in {
      inside(cpg.call.nameExact(Operators.indexAccess).codeExact("xs[i][j]").argument(1).l) {
        case (inner: Call) :: Nil =>
          inner.name shouldBe Operators.indexAccess
          inner.code shouldBe "xs[i]"
      }
    }

    "have the index as the second argument of the outer indexAccess" in {
      inside(cpg.call.nameExact(Operators.indexAccess).codeExact("xs[i][j]").argument(2).l) {
        case (j: Identifier) :: Nil =>
          j.name shouldBe "j"
          j.typeFullName shouldBe "usize"
      }
    }
  }

  "a field access on `self`" should {
    val cpg = code("""
        |struct Foo {
        | bar: i32,
        |}
        |impl Foo {
        | fn get_bar(&self) -> i32 {
        |   self.bar
        | }
        |}
        |""".stripMargin)

    "lower to a fieldAccess call" in {
      inside(cpg.call.nameExact(Operators.fieldAccess).l) { case fieldAccess :: Nil =>
        fieldAccess.code shouldBe "self.bar"
        fieldAccess.methodFullName shouldBe Operators.fieldAccess
        fieldAccess.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        fieldAccess.typeFullName shouldBe "i32"
      }
    }

    "have the lhs as the first argument" in {
      inside(cpg.call.nameExact(Operators.fieldAccess).argument(1).l) { case (deref: Call) :: Nil =>
        deref.name shouldBe Operators.indirection
        deref.code shouldBe "*self"
        deref.typeFullName shouldBe "rust2cpgtest::Foo"
        inside(deref.argument.l) { case (self: Identifier) :: Nil =>
          self.name shouldBe "self"
          self.code shouldBe "self"
          self.typeFullName shouldBe "&rust2cpgtest::Foo"
        }
      }
    }

    "have the field as the second argument" in {
      inside(cpg.call.nameExact(Operators.fieldAccess).argument(2).l) { case (field: FieldIdentifier) :: Nil =>
        field.code shouldBe "bar"
        field.canonicalName shouldBe "bar"
      }
    }
  }

  "unary operators" should {
    val cpg = code("""
        |fn main(x: i32, b: bool, p: *const i32) {
        | let a = -x;
        | let c = !b;
        | let d = *p;
        |}
        |""".stripMargin)

    "lower `-x` to a minus call with i32 typeFullName" in {
      inside(cpg.call.nameExact(Operators.minus).l) { case minus :: Nil =>
        minus.code shouldBe "-x"
        minus.methodFullName shouldBe Operators.minus
        minus.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        minus.typeFullName shouldBe "i32"
      }
    }

    "have the i32 operand as the argument of `-x`" in {
      inside(cpg.call.nameExact(Operators.minus).argument.l) { case (x: Identifier) :: Nil =>
        x.name shouldBe "x"
        x.typeFullName shouldBe "i32"
      }
    }

    "lower `!b` to a logicalNot call with bool typeFullName" in {
      inside(cpg.call.nameExact(Operators.logicalNot).l) { case logicalNot :: Nil =>
        logicalNot.code shouldBe "!b"
        logicalNot.methodFullName shouldBe Operators.logicalNot
        logicalNot.typeFullName shouldBe "bool"
      }
    }

    "have the bool operand as the argument of `!b`" in {
      inside(cpg.call.nameExact(Operators.logicalNot).argument.l) { case (b: Identifier) :: Nil =>
        b.name shouldBe "b"
        b.typeFullName shouldBe "bool"
      }
    }

    "lower `*p` to an indirection call with i32 typeFullName" in {
      inside(cpg.call.nameExact(Operators.indirection).l) { case indirection :: Nil =>
        indirection.code shouldBe "*p"
        indirection.methodFullName shouldBe Operators.indirection
        indirection.typeFullName shouldBe "i32"
      }
    }

    "have the pointer operand as the argument of `*p`" in {
      inside(cpg.call.nameExact(Operators.indirection).argument.l) { case (p: Identifier) :: Nil =>
        p.name shouldBe "p"
        p.typeFullName shouldBe "*const i32"
      }
    }
  }

  "a shared reference expression" should {
    val cpg = code("""
        |fn main(x: i32) {
        | let r = &x;
        |}
        |""".stripMargin)

    "lower `&x` as an addressOf call with &i32 typeFullName" in {
      inside(cpg.call.nameExact(Operators.addressOf).l) { case addressOf :: Nil =>
        addressOf.code shouldBe "&x"
        addressOf.methodFullName shouldBe Operators.addressOf
        addressOf.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        addressOf.typeFullName shouldBe "&i32"
      }
    }

    "have correct arguments in `&x`" in {
      inside(cpg.call.nameExact(Operators.addressOf).argument.l) { case (ident: Identifier) :: Nil =>
        ident.name shouldBe "x"
        ident.typeFullName shouldBe "i32"
      }
    }
  }

  "a mutable reference expression" should {
    val cpg = code("""
        |fn main(mut x: i32) {
        | let m = &mut x;
        |}
        |""".stripMargin)

    "lower `&mut x` as an addressOf call with &mut i32 typeFullName" in {
      inside(cpg.call.nameExact(Operators.addressOf).l) { case addressOf :: Nil =>
        addressOf.code shouldBe "&mut x"
        addressOf.methodFullName shouldBe Operators.addressOf
        addressOf.typeFullName shouldBe "&mut i32"
      }
    }

    "have correct arguments in `&mut x`" in {
      inside(cpg.call.nameExact(Operators.addressOf).argument.l) { case (ident: Identifier) :: Nil =>
        ident.name shouldBe "x"
        ident.typeFullName shouldBe "i32"
      }
    }
  }

  "a raw reference expression" should {
    val cpg = code("""
        |fn main(x: i32) {
        | let p = &raw const x;
        |}
        |""".stripMargin)

    "lower `&raw const x` as an addressOf call with *const i32 typeFullName" in {
      inside(cpg.call.nameExact(Operators.addressOf).l) { case addressOf :: Nil =>
        addressOf.code shouldBe "&raw const x"
        addressOf.methodFullName shouldBe Operators.addressOf
        addressOf.typeFullName shouldBe "*const i32"
      }
    }

    "have correct arguments in `&raw const x`" in {
      inside(cpg.call.nameExact(Operators.addressOf).argument.l) { case (ident: Identifier) :: Nil =>
        ident.name shouldBe "x"
        ident.typeFullName shouldBe "i32"
      }
    }
  }

  "nested arithmetic and comparison operators" should {
    val cpg = code("""
        |fn main(x: i32, y: i32) -> bool {
        | (x + y) > 0
        |}
        |""".stripMargin)

    "have an i32 typeFullName for the addition" in {
      cpg.call.nameExact(Operators.addition).typeFullName.l shouldBe List("i32")
    }

    "have a bool typeFullName for the comparison" in {
      cpg.call.nameExact(Operators.greaterThan).typeFullName.l shouldBe List("bool")
    }
  }
}
