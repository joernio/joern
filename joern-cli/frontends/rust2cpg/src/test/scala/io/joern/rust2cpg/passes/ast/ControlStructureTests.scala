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

    "place bar in the else-branch" in {
      cpg.ifBlock.whenFalse.isBlock.astChildren.isCall.name.l shouldBe List("bar")
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

    "place the inner IF directly in the outer else-branch" in {
      inside(cpg.ifBlock.condition("x < y").whenFalse.l) { case (innerIf: ControlStructure) :: Nil =>
        innerIf.controlStructureType shouldBe ControlStructureTypes.IF
        innerIf.condition.code.l shouldBe List("x == y")
      }
    }

    "place baz in the inner else-branch" in {
      inside(cpg.ifBlock.condition("x == y").whenFalse.isBlock.l) { case innerElse :: Nil =>
        innerElse.astChildren.isCall.name.l shouldBe List("baz")
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

  "if-let tail expression" should {
    val cpg = code("""
        |fn main() {
        | if let Some(x) = foo() {
        |  sink(x);
        | }
        |}
        |""".stripMargin)

    "have correct locals and assignments" in {
      inside(cpg.method.nameExact("main").body.astChildren.isReturn.astChildren.isBlock.astChildren.sortBy(_.order).l) {
        case (tmpLocal: Local) :: (tmpAssign: Call) :: (ifNode: ControlStructure) :: Nil =>
          tmpLocal.name shouldBe "<tmp>0"
          tmpAssign.code shouldBe "<tmp>0 = foo()"
          ifNode.code shouldBe "if let Some(x) = foo() {\n  sink(x);\n }"
      }
    }

    "have correct condition" in {
      inside(cpg.ifBlock.condition.l) { case (condition: Unknown) :: Nil =>
        condition.code shouldBe "Some(x)"
      }
    }

    "have correct then-branch" in {
      inside(cpg.ifBlock.whenTrue.isBlock.astChildren.sortBy(_.order).l) {
        case (xLocal: Local) :: (xAssign: Call) :: (body: Call) :: Nil =>
          xLocal.name shouldBe "x"
          xAssign.code shouldBe "x = <tmp>0.0"
          body.code shouldBe "sink(x)"
      }
    }

    "have correct REF edges" in {
      cpg.local.nameExact("x").referencingIdentifiers.lineNumber.l shouldBe List(3, 4)
    }

    "have no else-branch" in {
      cpg.ifBlock.whenFalse shouldBe empty
    }
  }

  "if-let-else tail expression" should {
    val cpg = code("""
        |fn main() {
        | if let Some(x) = foo() {
        |  sink(x);
        | } else {
        |  bar();
        | }
        |}
        |""".stripMargin)

    "have correct then-branch" in {
      cpg.ifBlock.whenTrue.isBlock.astChildren.isCall.code.l shouldBe List("x = <tmp>0.0", "sink(x)")
    }

    "have correct else-branch" in {
      cpg.ifBlock.whenFalse.isBlock.astChildren.isCall.code.l shouldBe List("bar()")
    }
  }

  "if-let with _" should {
    val cpg = code("""
        |fn main() {
        | if let Some(_) = foo() {
        |  bar();
        | }
        |}
        |""".stripMargin)

    "have correct then-branch" in {
      inside(cpg.ifBlock.whenTrue.isBlock.astChildren.l) { case (body: Call) :: Nil =>
        body.code shouldBe "bar()"
      }
    }
  }

  "if-let with record struct" should {
    val cpg = code("""
        |struct Shape { w: i32, h: i32 }
        |fn main(shape: Shape) {
        | if let Shape { w, h } = shape {
        |  sink(w, h);
        | }
        |}
        |""".stripMargin)

    "have correct then-branch" in {
      inside(cpg.ifBlock.whenTrue.isBlock.astChildren.sortBy(_.order).l) {
        case (wLocal: Local) :: (hLocal: Local) :: (wAssign: Call) :: (hAssign: Call) :: (body: Call) :: Nil =>
          wLocal.name shouldBe "w"
          wLocal.typeFullName shouldBe "i32"
          hLocal.name shouldBe "h"
          hLocal.typeFullName shouldBe "i32"
          wAssign.code shouldBe "w = <tmp>0.w"
          hAssign.code shouldBe "h = <tmp>0.h"
          body.code shouldBe "sink(w, h)"
      }
    }
  }

  "if-let shadowing previous let" should {
    val cpg = code("""
        |fn main() {
        | let x = 1;
        | if let Some(x) = foo() {
        |  sink(x);
        | } else {
        |  sink(x);
        | }
        |}
        |""".stripMargin)

    "have correct locals" in {
      cpg.local.nameExact("x").lineNumber.l shouldBe List(3, 4)
    }

    "have correct REF edges for each local" in {
      cpg.local.nameExact("x").lineNumber(3).referencingIdentifiers.lineNumber.l shouldBe List(3, 7)
      cpg.local.nameExact("x").lineNumber(4).referencingIdentifiers.lineNumber.l shouldBe List(4, 5)
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
