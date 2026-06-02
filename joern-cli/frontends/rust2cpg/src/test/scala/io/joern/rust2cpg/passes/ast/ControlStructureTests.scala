package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class ControlStructureTests extends Rust2CpgSuite(noSysRoot = true) {

  "an if without an else" should {
    val cpg = code("""
        |fn main(x: i32, y: i32) {
        | if x > y {
        |  foo();
        | }
        |}
        |""".stripMargin)

    "have correct code" in {
      cpg.ifBlock.code.l shouldBe List("if x > y {\n  foo();\n }")
    }

    "lower the condition as a > call" in {
      inside(cpg.ifBlock.condition.isCall.l) { case condition :: Nil =>
        condition.code shouldBe "x > y"
        condition.name shouldBe Operators.greaterThan
        condition.methodFullName shouldBe Operators.greaterThan
      }
    }

    "have x and y as arguments to the > call" in {
      cpg.ifBlock.condition.isCall.argument.isIdentifier.name.l shouldBe List("x", "y")
    }

    "have x and y reference the parameters" in {
      cpg.identifier("x").refsTo.l shouldBe cpg.parameter("x").l
      cpg.identifier("y").refsTo.l shouldBe cpg.parameter("y").l
    }

    "place foo in the then-branch" in {
      cpg.ifBlock.whenTrue.isBlock.astChildren.isCall.name.l shouldBe List("foo")
    }

    "have no else-branch" in {
      cpg.ifBlock.whenFalse shouldBe empty
    }
  }

  "an if with an else" should {
    val cpg = code("""
        |fn main(x: i32, y: i32) {
        | if x == y {
        |  foo();
        | } else {
        |  bar();
        | }
        |}
        |""".stripMargin)

    "lower the condition as an == call" in {
      inside(cpg.ifBlock.condition.isCall.l) { case condition :: Nil =>
        condition.code shouldBe "x == y"
        condition.name shouldBe Operators.equals
        condition.methodFullName shouldBe Operators.equals
      }
    }

    "place foo in the then-branch" in {
      cpg.ifBlock.whenTrue.isBlock.astChildren.isCall.name.l shouldBe List("foo")
    }

    "place bar in the ELSE body" in {
      cpg.elseBlock.astChildren.isBlock.astChildren.isCall.name.l shouldBe List("bar")
    }
  }

  "an else-if chain" should {
    val cpg = code("""
        |fn main(x: i32, y: i32) {
        | if x < y {
        |  foo();
        | } else if x == y {
        |  bar();
        | } else {
        |  baz();
        | }
        |}
        |""".stripMargin)

    "have one IF per if" in {
      cpg.ifBlock.size shouldBe 2
    }

    "place the inner IF inside the outer ELSE" in {
      inside(cpg.ifBlock.condition("x < y").whenFalse.l) { case (outerElse: ControlStructure) :: Nil =>
        outerElse.controlStructureType shouldBe ControlStructureTypes.ELSE
        inside(outerElse.astChildren.l) { case (innerIf: ControlStructure) :: Nil =>
          innerIf.controlStructureType shouldBe ControlStructureTypes.IF
          innerIf.condition.code.l shouldBe List("x == y")
        }
      }
    }

    "place baz inside the inner ELSE" in {
      inside(cpg.ifBlock.condition("x == y").whenFalse.l) { case (innerElse: ControlStructure) :: Nil =>
        innerElse.controlStructureType shouldBe ControlStructureTypes.ELSE
        innerElse.astChildren.isBlock.astChildren.isCall.name.l shouldBe List("baz")
      }
    }
  }

  "a nested if" should {
    val cpg = code("""
        |fn main(x: i32, y: i32) {
        | if x < y {
        |  if x == 0 {
        |   foo();
        |  }
        | }
        |}
        |""".stripMargin)

    "have one IF per if" in {
      cpg.ifBlock.size shouldBe 2
    }

    "place the inner IF in the outer then-branch" in {
      cpg.ifBlock
        .condition("x < y")
        .whenTrue
        .isBlock
        .astChildren
        .isControlStructure
        .isIf
        .condition
        .code
        .l shouldBe List("x == 0")
    }
  }

  "a while loop" should {
    val cpg = code("""
        |fn main(x: i32, y: i32) {
        | while x < y {
        |  foo();
        | }
        |}
        |""".stripMargin)

    "have correct code" in {
      cpg.whileBlock.code.l shouldBe List("while x < y {\n  foo();\n }")
    }

    "lower the condition as a < call" in {
      inside(cpg.whileBlock.condition.isCall.l) { case condition :: Nil =>
        condition.code shouldBe "x < y"
        condition.name shouldBe Operators.lessThan
        condition.methodFullName shouldBe Operators.lessThan
      }
    }

    "have x and y as arguments to the < call" in {
      cpg.whileBlock.condition.isCall.argument.isIdentifier.name.l shouldBe List("x", "y")
    }

    "place foo in the loop body" in {
      cpg.whileBlock.astChildren.isBlock.astChildren.isCall.name.l shouldBe List("foo")
    }
  }

  "a loop expression" should {
    val cpg = code("""
        |fn main() {
        | loop {
        |  foo();
        |  break;
        | }
        |}
        |""".stripMargin)

    "lower as a WHILE with correct code" in {
      cpg.whileBlock.code.l shouldBe List("loop {\n  foo();\n  break;\n }")
    }

    "have a fake true literal as condition" in {
      inside(cpg.whileBlock.condition.isLiteral.l) { case condition :: Nil =>
        condition.code shouldBe "true"
        condition.typeFullName shouldBe "bool"
      }
    }

    "place foo in the loop body" in {
      cpg.whileBlock.astChildren.isBlock.astChildren.isCall.name.l shouldBe List("foo")
    }

    "place break in the loop body" in {
      cpg.whileBlock.astChildren.isBlock.astChildren.isControlStructure.isBreak.code.l shouldBe List("break")
    }
  }

  "continue and break inside a loop" should {
    val cpg = code("""
        |fn foo() -> i32 {
        | let x = 0;
        | loop {
        |  if x == 5 {
        |   continue;
        |  }
        |  break 1;
        | }
        |}
        |""".stripMargin)

    "lower continue as a CONTINUE" in {
      cpg.continue.code.l shouldBe List("continue")
    }

    "lower break 1 as a BREAK with the value in code" in {
      cpg.break.code.l shouldBe List("break 1")
    }
  }

  "a logical not as a condition" should {
    val cpg = code("""
        |fn main(b: bool) {
        | if !b {
        |  foo();
        | }
        |}
        |""".stripMargin)

    "lower to a logicalNot" in {
      inside(cpg.ifBlock.condition.isCall.l) { case condition :: Nil =>
        condition.code shouldBe "!b"
        condition.name shouldBe Operators.logicalNot
        condition.methodFullName shouldBe Operators.logicalNot
        condition.typeFullName shouldBe "bool"
      }
    }

    "have b as the single argument" in {
      inside(cpg.ifBlock.condition.isCall.argument.l) { case (b: Identifier) :: Nil =>
        b.code shouldBe "b"
        b.name shouldBe "b"
        b.typeFullName shouldBe "bool"
      }
    }
  }
}
