package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class ControlStructureTests extends Rust2CpgSuite(noSysRoot = true) {

  "`if` without `else` has an empty whenFalse branch" in {
    val cpg = code("""
        |fn main(x: i32, y: i32) {
        | if x > y {
        |  foo();
        | }
        |}
        |""".stripMargin)

    inside(cpg.method.name("main").controlStructure.controlStructureTypeExact(ControlStructureTypes.IF).l) {
      case (ifExpr: ControlStructure) :: Nil =>
        ifExpr.code shouldBe "if x > y {\n  foo();\n }"

        inside(ifExpr.condition.isCall.l) { case (condition: Call) :: Nil =>
          condition.code shouldBe "x > y"
          condition.name shouldBe Operators.greaterThan
          condition.methodFullName shouldBe Operators.greaterThan

          inside(condition.argument.l) { case (lhs: Identifier) :: (rhs: Identifier) :: Nil =>
            lhs.name shouldBe "x"
            rhs.name shouldBe "y"
          }
        }

        inside(ifExpr.whenTrue.isBlock.l) { case (thenBlock: Block) :: Nil =>
          thenBlock.astChildren.isCall.name.l shouldBe List("foo")
        }

        ifExpr.whenFalse.l shouldBe empty
    }
  }

  "`if/else` is lowered correctly" in {
    val cpg = code("""
        |fn main(x: i32, y: i32) {
        | if x == y {
        |  foo();
        | } else {
        |  bar();
        | }
        |}
        |""".stripMargin)

    inside(cpg.method.name("main").controlStructure.controlStructureTypeExact(ControlStructureTypes.IF).l) {
      case (ifExpr: ControlStructure) :: Nil =>
        inside(ifExpr.condition.isCall.l) { case (condition: Call) :: Nil =>
          condition.code shouldBe "x == y"
          condition.name shouldBe Operators.equals
          condition.methodFullName shouldBe Operators.equals
        }

        ifExpr.whenTrue.isBlock.astChildren.isCall.name.l shouldBe List("foo")

        inside(ifExpr.whenFalse.isControlStructure.controlStructureTypeExact(ControlStructureTypes.ELSE).l) {
          case (elseExpr: ControlStructure) :: Nil =>
            elseExpr.code shouldBe "else"
            elseExpr.astChildren.isBlock.astChildren.isCall.name.l shouldBe List("bar")
        }
    }
  }

  "`while` is lowered correctly" in {
    val cpg = code("""
        |fn main(x: i32, y: i32) {
        | while x < y {
        |  foo();
        | }
        |}
        |""".stripMargin)

    inside(cpg.method.name("main").controlStructure.controlStructureTypeExact(ControlStructureTypes.WHILE).l) {
      case (whileExpr: ControlStructure) :: Nil =>
        whileExpr.code shouldBe "while x < y {\n  foo();\n }"

        inside(whileExpr.condition.isCall.l) { case (condition: Call) :: Nil =>
          condition.code shouldBe "x < y"
          condition.name shouldBe Operators.lessThan
          condition.methodFullName shouldBe Operators.lessThan

          inside(condition.argument.l) { case (lhs: Identifier) :: (rhs: Identifier) :: Nil =>
            lhs.name shouldBe "x"
            rhs.name shouldBe "y"
          }
        }

        inside(whileExpr.astChildren.isBlock.l) { case (body: Block) :: Nil =>
          body.astChildren.isCall.name.l shouldBe List("foo")
        }
    }
  }

  "`loop` lowers as WHILE with a `true` literal condition" in {
    val cpg = code("""
        |fn main() {
        | loop {
        |  foo();
        |  break;
        | }
        |}
        |""".stripMargin)

    inside(cpg.method.name("main").controlStructure.controlStructureTypeExact(ControlStructureTypes.WHILE).l) {
      case (loopExpr: ControlStructure) :: Nil =>
        loopExpr.code shouldBe "loop {\n  foo();\n  break;\n }"

        inside(loopExpr.condition.isLiteral.l) { case (condition: Literal) :: Nil =>
          condition.code shouldBe "true"
          condition.typeFullName shouldBe "bool"
        }

        inside(loopExpr.astChildren.isBlock.l) { case (body: Block) :: Nil =>
          body.astChildren.isCall.name.l shouldBe List("foo")
          body.astChildren.isControlStructure
            .controlStructureTypeExact(ControlStructureTypes.BREAK)
            .code
            .l shouldBe List("break")
        }
    }
  }

  "`continue/break` inside a `while` lower as CONTINUE/BREAK control structures" in {
    val cpg = code("""
        |fn main(x: i32) {
        | while x < 10 {
        |  if x == 5 {
        |   continue;
        |  }
        |  break 1;
        | }
        |}
        |""".stripMargin)

    inside(cpg.method.name("main").controlStructure.controlStructureTypeExact(ControlStructureTypes.CONTINUE).l) {
      case (continueExpr: ControlStructure) :: Nil =>
        continueExpr.code shouldBe "continue"
        continueExpr.controlStructureType shouldBe ControlStructureTypes.CONTINUE
    }

    inside(cpg.method.name("main").controlStructure.controlStructureTypeExact(ControlStructureTypes.BREAK).l) {
      case (breakExpr: ControlStructure) :: Nil =>
        breakExpr.code shouldBe "break 1"
        breakExpr.controlStructureType shouldBe ControlStructureTypes.BREAK
    }
  }
}
