package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes

class AstCreationPassTests extends GoCodeToCpgSuite {

  "Method Ast layout" should {

    "be correct for decl assignment" ignore {
      val cpg = code("""
          |package main
          |func main() {
          |   var local int = 1
          |}
          |""".stripMargin)

      inside(cpg.method.name("main").block.astChildren.l) { case List(local: Local, call: Call) =>
        local.name shouldBe "local"
        local.typeFullName shouldBe "int"
        local.order shouldBe 1
        call.name shouldBe Operators.assignment
        call.order shouldBe 2
        inside(call.astChildren.l) { case List(identifier: Identifier, literal: Literal) =>
          identifier.name shouldBe "local"
          identifier.typeFullName shouldBe "int"
          identifier.order shouldBe 1
          identifier.argumentIndex shouldBe 1
          literal.code shouldBe "1"
          literal.typeFullName shouldBe "int"
          literal.order shouldBe 2
          literal.argumentIndex shouldBe 2
        }
      }
    }

    "be correct for nested expression" in {
      val cpg = code("""
          |package main
          |func method() {
          |  var x int
          |  var y int
          |  var z int
          |
          |  x = y + z
          |}
      """.stripMargin)

      val localX = cpg.local.order(1)
      localX.name.l shouldBe List("x")
      val localY = cpg.local.order(2)
      localY.name.l shouldBe List("y")
      val localZ = cpg.local.order(3)
      localZ.name.l shouldBe List("z")

      inside(cpg.method.name("method").ast.isCall.name(Operators.assignment).map(new OpNodes.Assignment(_)).l) {
        case List(assignment) =>
          assignment.target.code shouldBe "x"
          assignment.source.start.isCall.name.l shouldBe List(Operators.addition)
          inside(assignment.source.astChildren.l) { case List(id1: Identifier, id2: Identifier) =>
            id1.order shouldBe 1
            id1.code shouldBe "y"
            id2.order shouldBe 2
            id2.code shouldBe "z"
          }
      }
    }
  }

  "multiple declaration on single line" should {
    val cpg = code("""
        |package main
        |func main(){
        |   var  i, j int
        |   var  f, salary float32 = 10.0, 20.0
        |}
        |""".stripMargin)
    "create local and identifier nodes" in {
      val locals      = cpg.local.l
      val identifiers = cpg.identifier.l

      locals.size shouldBe 4
      locals.map(loc => (loc.name, loc.typeFullName)).l shouldBe List(
        ("i", "int"),
        ("j", "int"),
        ("f", "float32"),
        ("salary", "float32")
      )

      identifiers.size shouldBe 2
      identifiers.map(loc => (loc.name, loc.typeFullName)).l shouldBe List(("f", "float32"), ("salary", "float32"))
    }
  }

  "dynamic declaration" should {
    val cpg = code("""
        |package main
        |func main(){
        |   d := 43
        |   c := "value"
        |}
        |""".stripMargin)
    "have local and identifier nodes created" in {
      val locals      = cpg.local.l
      val identifiers = cpg.identifier.l

      locals.size shouldBe 2
      locals.map(loc => (loc.name, loc.typeFullName)).l shouldBe List(("d", "int"), ("c", "string"))

      identifiers.size shouldBe 2
      identifiers.map(loc => (loc.name, loc.typeFullName)).l shouldBe List(("d", "int"), ("c", "string"))
    }
  }

}
class AstCreationForOperatorsTests extends GoCodeToCpgSuite {

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
