package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.FileUtil.*

import java.nio.file.Paths

class SimpleAstCreationPassTest extends Rust2CpgSuite {

  "test 07" in {
    val cpg = code("""
        |fn main(x: i32) {
        | let y = x as i64;
        |}
        |""".stripMargin)

    inside(cpg.call.nameExact(Operators.cast).l) { case cast :: Nil =>
      cast.code shouldBe "x as i64"
      cast.methodFullName shouldBe Operators.cast
      cast.typeFullName shouldBe "i64"

      inside(cast.argument.l) { case (typeRef: TypeRef) :: (identifier: Identifier) :: Nil =>
        typeRef.code shouldBe "i64"
        typeRef.typeFullName shouldBe "i64"
        identifier.name shouldBe "x"
        identifier.code shouldBe "x"
      }
    }
  }

  "test 21" in {
    val cpg = code("""
        |fn foo(xs: Vec<i32>, i: usize) -> i32 {
        | xs[i]
        |}
        |""".stripMargin)

    inside(cpg.call.nameExact(Operators.indexAccess).l) { case indexAccess :: Nil =>
      indexAccess.code shouldBe "xs[i]"
      indexAccess.methodFullName shouldBe Operators.indexAccess
      indexAccess.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      inside(indexAccess.argument.l) { case base :: index :: Nil =>
        base.code shouldBe "xs"
        base.argumentIndex shouldBe 1
        index.code shouldBe "i"
        index.argumentIndex shouldBe 2
      }
    }
  }

  "test 22" in {
    val cpg = code("""
        |fn foo(xs: Vec<Vec<i32>>, i: usize, j: usize) -> i32 {
        | xs[i][j]
        |}
        |""".stripMargin)

    inside(cpg.call.nameExact(Operators.indexAccess).l.sortBy(_.code.length)) { case inner :: outer :: Nil =>
      inner.code shouldBe "xs[i]"
      outer.code shouldBe "xs[i][j]"

      inside(outer.argument.l) { case (innerArg: Call) :: (indexArg: Identifier) :: Nil =>
        innerArg shouldBe inner
        innerArg.argumentIndex shouldBe 1
        // TODO: innerArg.typeFullName shouldBe

        indexArg.code shouldBe "j"
        indexArg.argumentIndex shouldBe 2
      // TODO: indexArg.typeFullName shouldBe
      }
    }
  }

  "test 23" in {
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

  "test 24" in {
    val cpg = code("""
        |fn foo(pair: (i32, i32)) -> i32 {
        | pair.0
        |}
        |""".stripMargin)

    inside(cpg.call.nameExact(Operators.indexAccess).l) { case indexAccess :: Nil =>
      indexAccess.code shouldBe "pair.0"
      indexAccess.methodFullName shouldBe Operators.indexAccess
      indexAccess.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      inside(indexAccess.argument.l) { case (base: Identifier) :: (index: Literal) :: Nil =>
        base.code shouldBe "pair"
        base.argumentIndex shouldBe 1

        index.code shouldBe "0"
        index.argumentIndex shouldBe 2
        index.typeFullName shouldBe "i32"
      }
    }
  }

}
