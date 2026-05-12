package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class OperatorTests extends Rust2CpgSuite(noSysRoot = true) {

  "`as` lowers to `<operator>.cast`" in {
    val cpg = code("""
        |fn main(x: i32) {
        | let y = x as i64;
        |}
        |""".stripMargin)

    inside(cpg.call.nameExact(Operators.cast).l) { case cast :: Nil =>
      cast.code shouldBe "x as i64"
      cast.methodFullName shouldBe Operators.cast
      cast.typeFullName shouldBe "i64"

      inside(cast.argument.sortBy(_.argumentIndex).l) { case (typeRef: TypeRef) :: (identifier: Identifier) :: Nil =>
        typeRef.code shouldBe "i64"
        typeRef.typeFullName shouldBe "i64"
        typeRef.argumentIndex shouldBe 1

        identifier.name shouldBe "x"
        identifier.code shouldBe "x"
        identifier.typeFullName shouldBe "i32"
        identifier.argumentIndex shouldBe 2
      }
    }
  }

  "`[i]` lowers to `<operator>.indexAccess`" in {
    val cpg = code("""
        |fn foo(xs: Vec<i32>, i: usize) -> i32 {
        | xs[i]
        |}
        |""".stripMargin)

    inside(cpg.call.nameExact(Operators.indexAccess).l) { case indexAccess :: Nil =>
      indexAccess.code shouldBe "xs[i]"
      indexAccess.methodFullName shouldBe Operators.indexAccess
      indexAccess.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      indexAccess.typeFullName shouldBe "i32"

      inside(indexAccess.argument.l) { case (xs: Identifier) :: (i: Identifier) :: Nil =>
        xs.code shouldBe "xs"
        xs.argumentIndex shouldBe 1
        xs.name shouldBe "xs"
        // TODO (rust_ast_gen): confirm why it doesn't have a type
        xs.typeFullName shouldBe Defines.Any

        i.code shouldBe "i"
        i.argumentIndex shouldBe 2
        i.typeFullName shouldBe "usize"
      }
    }
  }

  "`[i][j]` lowers to nested `<operator>.indexAccess`" in {
    val cpg = code("""
        |fn foo(xs: Vec<Vec<i32>>, i: usize, j: usize) -> i32 {
        | xs[i][j]
        |}
        |""".stripMargin)

    inside(cpg.call.nameExact(Operators.indexAccess).l.sortBy(_.code.length)) { case inner :: outer :: Nil =>
      inner.code shouldBe "xs[i]"
      outer.code shouldBe "xs[i][j]"
      outer.typeFullName shouldBe "i32"
      // TODO (rust_ast_gen): confirm that no type for the inner is to be expected
      inner.typeFullName shouldBe Defines.Any

      inside(outer.argument.l) { case (innerArg: Call) :: (indexArg: Identifier) :: Nil =>
        innerArg shouldBe inner
        innerArg.argumentIndex shouldBe 1
        innerArg.typeFullName shouldBe Defines.Any

        indexArg.code shouldBe "j"
        indexArg.argumentIndex shouldBe 2
        indexArg.typeFullName shouldBe "usize"
      }
    }
  }

  "named struct field access lowers to `<operator>.fieldAccess`" in {
    val cpg = code("""
        |struct Point {
        | x: i32,
        |}
        |
        |fn foo(point: Point) -> i32 {
        | point.x
        |}
        |""".stripMargin)

    inside(cpg.call.nameExact(Operators.fieldAccess).l) { case fieldAccess :: Nil =>
      fieldAccess.code shouldBe "point.x"
      fieldAccess.methodFullName shouldBe Operators.fieldAccess
      fieldAccess.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      inside(fieldAccess.argument.l) { case (base: Identifier) :: (field: FieldIdentifier) :: Nil =>
        base.code shouldBe "point"
        base.argumentIndex shouldBe 1

        field.code shouldBe "x"
        field.canonicalName shouldBe "x"
        field.argumentIndex shouldBe 2
      }
    }
  }

  "tuple positional access lowers to `<operator>.fieldAccess` with the index as field name" in {
    val cpg = code("""
        |fn foo(pair: (i32, i32)) -> i32 {
        | pair.0
        |}
        |""".stripMargin)

    inside(cpg.call.nameExact(Operators.fieldAccess).l) { case fieldAccess :: Nil =>
      fieldAccess.code shouldBe "pair.0"
      fieldAccess.methodFullName shouldBe Operators.fieldAccess
      fieldAccess.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      inside(fieldAccess.argument.l) { case (base: Identifier) :: (field: FieldIdentifier) :: Nil =>
        base.code shouldBe "pair"
        base.argumentIndex shouldBe 1

        field.code shouldBe "0"
        field.canonicalName shouldBe "0"
        field.argumentIndex shouldBe 2
      }
    }
  }

  "tuple-struct positional access lowers to `<operator>.fieldAccess` with the index as field name" in {
    val cpg = code("""
        |struct Pair(i32, bool);
        |
        |fn foo(pair: Pair) -> i32 {
        | pair.0
        |}
        |""".stripMargin)

    inside(cpg.call.nameExact(Operators.fieldAccess).l) { case fieldAccess :: Nil =>
      fieldAccess.code shouldBe "pair.0"
      fieldAccess.methodFullName shouldBe Operators.fieldAccess
      fieldAccess.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      inside(fieldAccess.argument.l) { case (base: Identifier) :: (field: FieldIdentifier) :: Nil =>
        base.code shouldBe "pair"
        base.argumentIndex shouldBe 1

        field.code shouldBe "0"
        field.canonicalName shouldBe "0"
        field.argumentIndex shouldBe 2
      }
    }
  }

  "unary `-`, `!` and `*` lower to minus, logicalNot, and indirection calls" in {
    val cpg = code("""
        |fn main(x: i32, b: bool, p: *const i32) {
        | let a = -x;
        | let c = !b;
        | let d = *p;
        |}
        |""".stripMargin)

    inside(cpg.call.nameExact(Operators.minus).l) { case minus :: Nil =>
      minus.code shouldBe "-x"
      minus.methodFullName shouldBe Operators.minus
      minus.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      minus.typeFullName shouldBe "i32"

      inside(minus.argument.l) { case (x: Identifier) :: Nil =>
        x.code shouldBe "x"
        x.argumentIndex shouldBe 1
        x.typeFullName shouldBe "i32"
      }
    }

    inside(cpg.call.nameExact(Operators.logicalNot).l) { case logicalNot :: Nil =>
      logicalNot.code shouldBe "!b"
      logicalNot.methodFullName shouldBe Operators.logicalNot
      logicalNot.typeFullName shouldBe "bool"

      inside(logicalNot.argument.l) { case (b: Identifier) :: Nil =>
        b.code shouldBe "b"
        b.argumentIndex shouldBe 1
        b.typeFullName shouldBe "bool"
      }
    }

    inside(cpg.call.nameExact(Operators.indirection).l) { case indirection :: Nil =>
      indirection.code shouldBe "*p"
      indirection.methodFullName shouldBe Operators.indirection
      indirection.typeFullName shouldBe "i32"

      inside(indirection.argument.l) { case (p: Identifier) :: Nil =>
        p.code shouldBe "p"
        p.argumentIndex shouldBe 1
        p.typeFullName shouldBe "*const i32"
      }
    }
  }

  "nested arithmetic and comparison operators have correct typeFullName" in {
    val cpg = code("""
        |fn main(x: i32, y: i32) -> bool {
        | (x + y) > 0
        |}
        |""".stripMargin)

    cpg.call.nameExact(Operators.addition).typeFullName.l shouldBe List("i32")
    cpg.call.nameExact(Operators.greaterThan).typeFullName.l shouldBe List("bool")
  }
}
