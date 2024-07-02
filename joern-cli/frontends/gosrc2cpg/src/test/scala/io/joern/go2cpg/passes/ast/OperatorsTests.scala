package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, Literal}
import io.shiftleft.semanticcpg.language.*

class OperatorsTests extends GoCodeToCpgSuite {

  "Ast nodes for Operators" should {
    "be created for binary operators" in {
      val cpg = code("""
                       |package main
                       |func main() {
                       |	a := 10
                       |	b := 5
                       |
                       |	// Arithmetic Operators
                       |	addition := a + b
                       |	subtraction := a - b
                       |	multiplication := a * b
                       |	division := a / b
                       |	modulus := a % b
                       |
                       |	// Comparison Operators
                       |	equalTo := a == b
                       |	notEqualTo := a != b
                       |	greaterThan := a > b
                       |	lessThan := a < b
                       |	greaterThanOrEqualTo := a >= b
                       |	lessThanOrEqualTo := a <= b
                       |
                       |	// Logical Operators
                       |	logicalAND := a && b
                       |	logicalOR := a || b
                       |
                       |	// Bitwise Operators
                       |	bitwiseAND := a & b
                       |	bitwiseOR := a | b
                       |	bitwiseXOR := a ^ b
                       |	bitwiseLeftShift := a << 2
                       |	bitwiseRightShift := a >> 2
                       |
                       |}
                       |
                       |""".stripMargin)

      val operatorCalls = cpg.method("main").ast.isCall.nameNot(Operators.assignment).l
      operatorCalls.size shouldBe 18
      operatorCalls.name.l shouldBe List(
        "<operator>.addition",
        "<operator>.subtraction",
        "<operator>.multiplication",
        "<operator>.division",
        "<operator>.modulo",
        "<operator>.equals",
        "<operator>.notEquals",
        "<operator>.greaterThan",
        "<operator>.lessThan",
        "<operator>.greaterEqualsThan",
        "<operator>.lessEqualsThan",
        "<operator>.logicalAnd",
        "<operator>.logicalOr",
        "<operator>.and",
        "<operator>.or",
        "<operator>.xor",
        "<operator>.shiftLeft",
        "<operator>.arithmeticShiftRight"
      )

      inside(operatorCalls.nameExact(Operators.addition).astChildren.l) { case List(lhs: Identifier, rhs: Identifier) =>
        lhs.name shouldBe "a"
        rhs.name shouldBe "b"
      }

      inside(operatorCalls.nameExact(Operators.shiftLeft).astChildren.l) { case List(lhs: Identifier, rhs: Literal) =>
        lhs.name shouldBe "a"
        rhs.code shouldBe "2"
      }

    }

    "be created for short hand assignment operators" in {
      val cpg = code("""
                       |package main
                       |func main() {
                       |	a := 10
                       |	b := 5
                       |
                       | a += b
                       | a /= b
                       | a %= b
                       | a *= b
                       | a -= b
                       | a <<= b
                       | a >>= b
                       | a &= b
                       | a ^= b
                       | a |= b
                       |}
                       |
                       |""".stripMargin)

      val operatorCalls = cpg.method("main").ast.isCall.nameNot(Operators.assignment).l
      operatorCalls.size shouldBe 10
      operatorCalls.name.l shouldBe List(
        "<operator>.assignmentPlus",
        "<operator>.assignmentDivision",
        "<operators>.assignmentModulo",
        "<operator>.assignmentMultiplication",
        "<operator>.assignmentMinus",
        "<operators>.assignmentShiftLeft",
        "<operators>.assignmentArithmeticShiftRight",
        "<operators>.assignmentAnd",
        "<operators>.assignmentXor",
        "<operators>.assignmentOr"
      )

      inside(operatorCalls.nameExact(Operators.assignmentModulo).astChildren.l) {
        case List(lhs: Identifier, rhs: Identifier) =>
          lhs.name shouldBe "a"
          rhs.name shouldBe "b"
      }

      inside(operatorCalls.nameExact(Operators.assignmentShiftLeft).astChildren.l) {
        case List(lhs: Identifier, rhs: Identifier) =>
          lhs.name shouldBe "a"
          rhs.code shouldBe "b"
      }

    }

    "be created for post increment/decrement statement" in {
      val cpg = code("""
                       |package main
                       |func main() {
                       |	a := 10
                       |	b := 5
                       |
                       | a++
                       | a--
                       |}
                       |
                       |""".stripMargin)

      val operatorCalls = cpg.method("main").ast.isCall.nameNot(Operators.assignment).l
      operatorCalls.size shouldBe 2
      operatorCalls.name.l shouldBe List("<operator>.postIncrement", "<operator>.postDecrement")

      inside(operatorCalls.nameExact(Operators.postIncrement).astChildren.l) { case List(operand: Identifier) =>
        operand.name shouldBe "a"
      }

      inside(operatorCalls.nameExact(Operators.postDecrement).astChildren.l) { case List(operand: Identifier) =>
        operand.name shouldBe "a"
        operand.typeFullName shouldBe "int"
      }
    }

    "be created for unary operators" in {
      val cpg = code("""
                       |package main
                       |func main() {
                       |	a := 10
                       |	b := 5
                       |
                       | c := &b
                       | d = !b
                       | e := +b
                       | f := -b
                       | g := *b
                       |
                       |}
                       |
                       |""".stripMargin)

      val operatorCalls = cpg.method("main").ast.isCall.nameNot(Operators.assignment).l
      operatorCalls.size shouldBe 5
      operatorCalls.name.l shouldBe List(
        "<operator>.addressOf",
        "<operator>.logicalNot",
        "<operator>.plus",
        "<operator>.minus",
        "<operator>.indirection"
      )

      inside(operatorCalls.nameExact(Operators.addressOf).astChildren.l) { case List(operand: Identifier) =>
        operand.name shouldBe "b"
        operand.typeFullName shouldBe "int"
      }
    }
  }
}
