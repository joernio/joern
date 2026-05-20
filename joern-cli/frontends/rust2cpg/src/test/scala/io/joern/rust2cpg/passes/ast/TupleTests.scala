package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class TupleTests extends Rust2CpgSuite(noSysRoot = true) {

  private val tupleLiteral = "<operator>.tupleLiteral"

  "a 2-tuple literal" should {
    val cpg = code("""
        |fn main() {
        | let p = (1, 2);
        |}
        |""".stripMargin)

    "lower to a tupleLiteral call" in {
      inside(cpg.call.nameExact(tupleLiteral).l) { case tuple :: Nil =>
        tuple.code shouldBe "(1, 2)"
        tuple.methodFullName shouldBe tupleLiteral
        tuple.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      }
    }

    "type the tupleLiteral as (i32, i32)" in {
      cpg.call.nameExact(tupleLiteral).typeFullName.l shouldBe List("(i32, i32)")
    }

    "have the i32 literals as arguments" in {
      inside(cpg.call.nameExact(tupleLiteral).argument.l) { case (a: Literal) :: (b: Literal) :: Nil =>
        a.code shouldBe "1"
        a.argumentIndex shouldBe 1
        a.typeFullName shouldBe "i32"

        b.code shouldBe "2"
        b.argumentIndex shouldBe 2
        b.typeFullName shouldBe "i32"
      }
    }
  }

  "a 3-tuple literal of mixed types" should {
    val cpg = code("""
        |fn main() {
        | let t = (1, "x", true);
        |}
        |""".stripMargin)

    "type the tupleLiteral with the element types" in {
      cpg.call.nameExact(tupleLiteral).typeFullName.l shouldBe List("(i32, &str, bool)")
    }

    "have the i32 literal as the first argument" in {
      inside(cpg.call.nameExact(tupleLiteral).argument(1).l) { case (i: Literal) :: Nil =>
        i.code shouldBe "1"
        i.typeFullName shouldBe "i32"
      }
    }

    "have the &str literal as the second argument" in {
      inside(cpg.call.nameExact(tupleLiteral).argument(2).l) { case (s: Literal) :: Nil =>
        s.code shouldBe "\"x\""
        s.typeFullName shouldBe "&str"
      }
    }

    "have the bool literal as the third argument" in {
      inside(cpg.call.nameExact(tupleLiteral).argument(3).l) { case (b: Literal) :: Nil =>
        b.code shouldBe "true"
        b.typeFullName shouldBe "bool"
      }
    }
  }

  "a nested tuple literal" should {
    val cpg = code("""
        |fn main() {
        | let n = ((1, 2), 3);
        |}
        |""".stripMargin)

    "lower as two tupleLiteral calls" in {
      cpg.call.nameExact(tupleLiteral).code.toSet shouldBe Set("(1, 2)", "((1, 2), 3)")
    }

    "type the outer tupleLiteral as ((i32, i32), i32)" in {
      cpg.call.nameExact(tupleLiteral).codeExact("((1, 2), 3)").typeFullName.l shouldBe List("((i32, i32), i32)")
    }

    "type the inner tupleLiteral as (i32, i32)" in {
      cpg.call.nameExact(tupleLiteral).codeExact("(1, 2)").typeFullName.l shouldBe List("(i32, i32)")
    }

    "have the inner tupleLiteral as the first argument of the outer" in {
      inside(cpg.call.nameExact(tupleLiteral).codeExact("((1, 2), 3)").argument(1).l) { case (inner: Call) :: Nil =>
        inner.code shouldBe "(1, 2)"
        inner.methodFullName shouldBe tupleLiteral
      }
    }

    "have the i32 literal as the second argument of the outer" in {
      inside(cpg.call.nameExact(tupleLiteral).codeExact("((1, 2), 3)").argument(2).l) { case (rest: Literal) :: Nil =>
        rest.code shouldBe "3"
        rest.typeFullName shouldBe "i32"
      }
    }
  }
}
