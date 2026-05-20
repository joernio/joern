package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.semanticcpg.language.*

class SimpleAstCreationPassTest extends Rust2CpgSuite {

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
