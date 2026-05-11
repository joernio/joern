package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class OperatorTests extends Rust2CpgSuite(noSysRoot = true) {

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

