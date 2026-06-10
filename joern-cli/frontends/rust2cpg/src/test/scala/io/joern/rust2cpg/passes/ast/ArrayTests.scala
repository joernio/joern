package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.astcreation.RustOperators
import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class ArrayTests extends Rust2CpgSuite(noSysRoot = true) {

  "an array literal" should {
    val cpg = code("""
        |fn main() {
        | let xs = [1, 2, 3];
        |}
        |""".stripMargin)

    "lower to an arrayInitializer call" in {
      inside(cpg.call.nameExact(Operators.arrayInitializer).l) { case array :: Nil =>
        array.code shouldBe "[1, 2, 3]"
        array.methodFullName shouldBe Operators.arrayInitializer
        array.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      }
    }

    "type the arrayInitializer as [i32; 3]" in {
      cpg.call.nameExact(Operators.arrayInitializer).typeFullName.l shouldBe List("[i32; 3]")
    }

    "have the i32 literals as arguments" in {
      inside(cpg.call.nameExact(Operators.arrayInitializer).argument.l) {
        case (one: Literal) :: (two: Literal) :: (three: Literal) :: Nil =>
          one.code shouldBe "1"
          one.argumentIndex shouldBe 1
          one.typeFullName shouldBe "i32"

          two.code shouldBe "2"
          two.argumentIndex shouldBe 2
          two.typeFullName shouldBe "i32"

          three.code shouldBe "3"
          three.argumentIndex shouldBe 3
          three.typeFullName shouldBe "i32"
      }
    }

    "have the arrayInitializer as the assignment's second argument" in {
      inside(cpg.assignment.argument(2).l) { case (rhs: Call) :: Nil =>
        rhs.name shouldBe Operators.arrayInitializer
        rhs.code shouldBe "[1, 2, 3]"
      }
    }
  }

  "an empty array literal" should {
    val cpg = code("""
        |fn main() {
        | let xs: [i32; 0] = [];
        |}
        |""".stripMargin)

    "lower to an arrayInitializer call with no arguments" in {
      inside(cpg.call.nameExact(Operators.arrayInitializer).l) { case array :: Nil =>
        array.code shouldBe "[]"
        array.methodFullName shouldBe Operators.arrayInitializer
        array.argument shouldBe empty
      }
    }

    "type the arrayInitializer as [i32; 0]" in {
      cpg.call.nameExact(Operators.arrayInitializer).typeFullName.l shouldBe List("[i32; 0]")
    }
  }

  "a repeat array literal" should {
    val cpg = code("""
        |fn main() {
        | let xs = [0; 5];
        |}
        |""".stripMargin)

    "lower to a repeatInArray call" in {
      inside(cpg.call.nameExact(RustOperators.repeatInArray).l) { case array :: Nil =>
        array.code shouldBe "[0; 5]"
        array.methodFullName shouldBe RustOperators.repeatInArray
        array.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      }
    }

    "type the repeatInArray as [i32; 5]" in {
      cpg.call.nameExact(RustOperators.repeatInArray).typeFullName.l shouldBe List("[i32; 5]")
    }

    "have the repeated value and the count as arguments" in {
      inside(cpg.call.nameExact(RustOperators.repeatInArray).argument.l) {
        case (value: Literal) :: (count: Literal) :: Nil =>
          value.code shouldBe "0"
          value.argumentIndex shouldBe 1
          value.typeFullName shouldBe "i32"

          count.code shouldBe "5"
          count.argumentIndex shouldBe 2
          count.typeFullName shouldBe "usize"
      }
    }
  }

  "a nested array literal" should {
    val cpg = code("""
        |fn main() {
        | let grid = [[1, 2], [3, 4]];
        |}
        |""".stripMargin)

    "lower as three arrayInitializer calls" in {
      cpg.call.nameExact(Operators.arrayInitializer).code.l shouldBe List("[[1, 2], [3, 4]]", "[1, 2]", "[3, 4]")
    }

    "have the inner arrays as arguments of the outer" in {
      inside(cpg.call.nameExact(Operators.arrayInitializer).codeExact("[[1, 2], [3, 4]]").argument.l) {
        case (first: Call) :: (second: Call) :: Nil =>
          first.name shouldBe Operators.arrayInitializer
          first.code shouldBe "[1, 2]"
          first.argumentIndex shouldBe 1

          second.name shouldBe Operators.arrayInitializer
          second.code shouldBe "[3, 4]"
          second.argumentIndex shouldBe 2
      }
    }
  }

  "an array literal of calls" should {
    val cpg = code("""
        |fn f() -> i32 { 1 }
        |fn g() -> i32 { 2 }
        |fn main() {
        | let xs = [f(), g()];
        |}
        |""".stripMargin)

    "have the calls as arguments" in {
      inside(cpg.call.nameExact(Operators.arrayInitializer).argument.l) { case (callF: Call) :: (callG: Call) :: Nil =>
        callF.name shouldBe "f"
        callF.code shouldBe "f()"
        callF.argumentIndex shouldBe 1

        callG.name shouldBe "g"
        callG.code shouldBe "g()"
        callG.argumentIndex shouldBe 2
      }
    }
  }
}
