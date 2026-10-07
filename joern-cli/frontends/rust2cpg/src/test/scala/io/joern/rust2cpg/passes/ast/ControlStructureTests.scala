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

  "if-else in let" should {
    val cpg = code("""
        |fn main(c: bool) {
        | let x = if c { 1 } else { 2 };
        |}
        |""".stripMargin)

    "have if wrapped in a block" in {
      inside(cpg.assignment.where(_.target.isIdentifier.nameExact("x")).source.l) { case (block: Block) :: Nil =>
        inside(block.astChildren.l) {
          case (tmpLocal: Local) :: (ifNode: ControlStructure) :: (tmpIdent: Identifier) :: Nil =>
            tmpLocal.name shouldBe "<tmp>0"
            tmpLocal.typeFullName shouldBe "i32"

            ifNode.controlStructureType shouldBe ControlStructureTypes.IF
            ifNode.code shouldBe "if c { 1 } else { 2 }"

            tmpIdent.name shouldBe "<tmp>0"
            tmpIdent.typeFullName shouldBe "i32"
        }
      }
    }

    "have correct then-branch" in {
      cpg.ifBlock.whenTrue.isBlock.astChildren.isCall.code.l shouldBe List("<tmp>0 = 1")
    }

    "have correct else-branch" in {
      cpg.ifBlock.whenFalse.isBlock.astChildren.isCall.code.l shouldBe List("<tmp>0 = 2")
    }
  }

  "if without else in let" should {
    val cpg = code("""
        |fn foo() {}
        |fn main(c: bool) {
        | let x = if c { foo() };
        |}
        |""".stripMargin)

    "have correct locals" in {
      inside(cpg.method.nameExact("main").local.l) { case local :: Nil =>
        local.name shouldBe "x"
        local.typeFullName shouldBe "()"
      }
    }

    "have correct assignment" in {
      inside(cpg.assignment.l) { case assign :: Nil =>
        assign.code shouldBe "let x = if c { foo() };"

        inside(assign.target) { case ident: Identifier =>
          ident.name shouldBe "x"
          ident.typeFullName shouldBe "()"
        }

        inside(assign.source) { case ifNode: ControlStructure =>
          ifNode.controlStructureType shouldBe ControlStructureTypes.IF
        }
      }
    }

    "have correct then-branch" in {
      inside(cpg.ifBlock.whenTrue.isBlock.astChildren.l) { case (foo: Call) :: Nil =>
        foo.name shouldBe "foo"
        foo.methodFullName shouldBe "rust2cpgtest::foo"
        foo.code shouldBe "foo()"
      }
    }
  }

  "nested if in let" should {
    val cpg = code("""
        |fn main(c: bool, d: bool) {
        | let x = if c { if d { 1 } else { 2 } } else { 3 };
        |}
        |""".stripMargin)

    "have correct locals" in {
      inside(cpg.local.sortBy(_.name).l) { case tmp0Local :: tmp1Local :: xLocal :: Nil =>
        tmp0Local.name shouldBe "<tmp>0"
        tmp0Local.typeFullName shouldBe "i32"

        tmp1Local.name shouldBe "<tmp>1"
        tmp1Local.typeFullName shouldBe "i32"

        xLocal.name shouldBe "x"
        xLocal.typeFullName shouldBe "i32"
      }
    }

    "have correct tmp assignments" in {
      cpg.assignment.where(_.target.isIdentifier.nameExact("<tmp>0")).code.l shouldBe List(
        "<tmp>0 = if d { 1 } else { 2 }",
        "<tmp>0 = 3"
      )
      cpg.assignment.where(_.target.isIdentifier.nameExact("<tmp>1")).code.l shouldBe List("<tmp>1 = 1", "<tmp>1 = 2")
    }

    "have correct then-branch" in {
      inside(cpg.ifBlock.condition("c").whenTrue.isBlock.astChildren.isCall.isAssignment.source.l) {
        case (ifDBlock: Block) :: Nil =>
          inside(ifDBlock.astChildren.l) {
            case (tmpLocal: Local) :: (ifD: ControlStructure) :: (tmpIdent: Identifier) :: Nil =>
              tmpLocal.name shouldBe "<tmp>1"
              ifD.controlStructureType shouldBe ControlStructureTypes.IF
              ifD.condition.code.l shouldBe List("d")
              ifD.whenTrue.isBlock.astChildren.isCall.code.l shouldBe List("<tmp>1 = 1")
              ifD.whenFalse.isBlock.astChildren.isCall.code.l shouldBe List("<tmp>1 = 2")
              tmpIdent.name shouldBe "<tmp>1"
          }
      }
    }

    "have correct else-branch" in {
      cpg.ifBlock.condition("c").whenFalse.isBlock.astChildren.isCall.code.l shouldBe List("<tmp>0 = 3")
    }
  }

  "nested else in let" should {
    val cpg = code("""
        |fn main(c: bool, d: bool) {
        | let x = if c { 1 } else if d { 2 } else { 3 };
        |}
        |""".stripMargin)

    "have correct locals" in {
      inside(cpg.local.sortBy(_.name).l) { case tmpLocal :: xLocal :: Nil =>
        tmpLocal.name shouldBe "<tmp>0"
        tmpLocal.typeFullName shouldBe "i32"

        xLocal.name shouldBe "x"
        xLocal.typeFullName shouldBe "i32"
      }
    }

    "have correct tmp assignments" in {
      cpg.assignment.where(_.target.isIdentifier.nameExact("<tmp>0")).code.l shouldBe List(
        "<tmp>0 = 1",
        "<tmp>0 = 2",
        "<tmp>0 = 3"
      )
    }

    "have correct then-branch" in {
      cpg.ifBlock.condition("c").whenTrue.isBlock.astChildren.isCall.code.l shouldBe List("<tmp>0 = 1")
    }

    "have correct else-branch" in {
      inside(cpg.ifBlock.condition("c").whenFalse.l) { case (ifD: ControlStructure) :: Nil =>
        ifD.controlStructureType shouldBe ControlStructureTypes.IF
        ifD.condition.code.l shouldBe List("d")
        ifD.whenTrue.isBlock.astChildren.isCall.code.l shouldBe List("<tmp>0 = 2")
        ifD.whenFalse.isBlock.astChildren.isCall.code.l shouldBe List("<tmp>0 = 3")
      }
    }
  }

  "if-else in assignment" should {
    val cpg = code("""
        |fn main(c: bool) {
        | let mut x = 0;
        | x = if c { 1 } else { 2 };
        |}
        |""".stripMargin)

    "have correct if wrapped in a block" in {
      inside(cpg.assignment.lineNumber(4).where(_.target.isIdentifier.nameExact("x")).source.l) {
        case (block: Block) :: Nil =>
          inside(block.astChildren.l) {
            case (tmpLocal: Local) :: (ifNode: ControlStructure) :: (tmpIdent: Identifier) :: Nil =>
              tmpLocal.name shouldBe "<tmp>0"
              ifNode.controlStructureType shouldBe ControlStructureTypes.IF
              tmpIdent.name shouldBe "<tmp>0"
          }
      }
    }

    "have correct then-branch" in {
      cpg.ifBlock.whenTrue.isBlock.astChildren.isCall.code.l shouldBe List("<tmp>0 = 1")
    }

    "have correct else-branch" in {
      cpg.ifBlock.whenFalse.isBlock.astChildren.isCall.code.l shouldBe List("<tmp>0 = 2")
    }
  }

  "if-else in += assignment" should {
    val cpg = code("""
        |fn main(c: bool) {
        | let mut x = 0;
        | x += if c { 1 } else { 2 };
        |}
        |""".stripMargin)

    "have correct result block" in {
      inside(cpg.call.nameExact(Operators.assignmentPlus).isAssignment.source.l) { case (block: Block) :: Nil =>
        inside(block.astChildren.l) {
          case (tmpLocal: Local) :: (ifNode: ControlStructure) :: (tmpIdent: Identifier) :: Nil =>
            tmpLocal.name shouldBe "<tmp>0"
            ifNode.controlStructureType shouldBe ControlStructureTypes.IF
            tmpIdent.name shouldBe "<tmp>0"
        }
      }
    }

    "have correct then-branch" in {
      cpg.ifBlock.whenTrue.isBlock.astChildren.isCall.code.l shouldBe List("<tmp>0 = 1")
    }

    "have correct else-branch" in {
      cpg.ifBlock.whenFalse.isBlock.astChildren.isCall.code.l shouldBe List("<tmp>0 = 2")
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

  "if-let chain" should {
    val cpg = code("""
        |fn main() {
        | if let Some(x) = foo() && x > 0 && let Some(y) = bar(x) {
        |  sink(y);
        | } else {
        |  baz();
        | };
        |}
        |""".stripMargin)

    "have correct locals" in {
      cpg.method.nameExact("main").block.astChildren.isBlock.astChildren.isLocal.name.l shouldBe
        List("<tmp>0", "x", "<tmp>1", "y")
    }

    "have correct assignments" in {
      cpg.method.nameExact("main").block.astChildren.isBlock.astChildren.isCall.code.l shouldBe
        List("<tmp>0 = foo()", "x = <tmp>0.0", "<tmp>1 = bar(x)", "y = <tmp>1.0")
    }

    "have correct condition" in {
      inside(cpg.ifBlock.condition.isCall.l) { case condition :: Nil =>
        condition.methodFullName shouldBe Operators.logicalAnd
        condition.code shouldBe "let Some(x) = foo() && x > 0 && let Some(y) = bar(x)"
      }

      inside(cpg.ifBlock.condition.isCall.argument.sortBy(_.argumentIndex).l) {
        case (lhs: Call) :: (rhs: Unknown) :: Nil =>
          lhs.methodFullName shouldBe Operators.logicalAnd
          lhs.code shouldBe "let Some(x) = foo() && x > 0"
          rhs.code shouldBe "Some(y)"
      }

      inside(cpg.ifBlock.condition.isCall.argument(1).isCall.argument.sortBy(_.argumentIndex).l) {
        case (lhs: Unknown) :: (rhs: Call) :: Nil =>
          lhs.code shouldBe "Some(x)"
          rhs.methodFullName shouldBe Operators.greaterThan
          rhs.code shouldBe "x > 0"
      }
    }

    "have correct then-branch" in {
      inside(cpg.ifBlock.whenTrue.isBlock.astChildren.sortBy(_.order).l) { case (body: Call) :: Nil =>
        body.code shouldBe "sink(y)"
      }
    }

    "have correct else-branch" in {
      inside(cpg.ifBlock.whenFalse.isBlock.astChildren.sortBy(_.order).l) { case (body: Call) :: Nil =>
        body.code shouldBe "baz()"
      }
    }

    "have correct REF edges" in {
      cpg.local.nameExact("x").referencingIdentifiers.lineNumber.l shouldBe List(3, 3, 3)
      cpg.local.nameExact("y").referencingIdentifiers.lineNumber.l shouldBe List(3, 4)
    }
  }

  "if-let chain shadowing previous let" should {
    val cpg = code("""
        |fn main() {
        | let x = 1;
        | if let Some(x) = foo() && x > 0 {
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
      cpg.local.nameExact("x").lineNumber(4).referencingIdentifiers.lineNumber.l shouldBe List(4, 4, 5)
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

  "while let" should {
    val cpg = code("""
        |fn main() {
        | while let Some(x) = foo() {
        |  bar(x);
        | }
        |}
        |""".stripMargin)

    "have correct condition" in {
      inside(cpg.whileBlock.condition.l) { case (condition: Unknown) :: Nil =>
        condition.code shouldBe "Some(x)"
      }
    }

    "have correct body" in {
      inside(cpg.whileBlock.whenTrue.isBlock.astChildren.sortBy(_.order).l) {
        case (tmpLocal: Local) :: (tmpAssign: Call) :: (xLocal: Local) :: (xAssign: Call) :: (body: Call) :: Nil =>
          tmpLocal.name shouldBe "<tmp>0"
          tmpAssign.code shouldBe "<tmp>0 = foo()"
          xLocal.name shouldBe "x"
          xAssign.code shouldBe "x = <tmp>0.0"
          body.code shouldBe "bar(x)"
      }
    }

    "have correct REF edges" in {
      cpg.local.nameExact("x").referencingIdentifiers.lineNumber.l shouldBe List(3, 4)
    }
  }

  "while let shadowing previous let" should {
    val cpg = code("""
        |fn main() {
        | let x = 1;
        | while let Some(x) = foo(x) {
        |  bar(x);
        | }
        |}
        |""".stripMargin)

    "have correct locals" in {
      cpg.local.nameExact("x").lineNumber.l shouldBe List(3, 4)
    }

    "have correct REF edges for each local" in {
      cpg.local.nameExact("x").lineNumber(3).referencingIdentifiers.lineNumber.l shouldBe List(3, 4)
      cpg.local.nameExact("x").lineNumber(4).referencingIdentifiers.lineNumber.l shouldBe List(4, 5)
    }
  }

  "while let over record struct" should {
    val cpg = code("""
        |struct Foo { x: i32, y: i32 }
        |fn main() {
        | while let Foo { x, y } = bar() {
        |  baz(x, y);
        | }
        |}
        |""".stripMargin)

    "have correct body" in {
      inside(cpg.whileBlock.whenTrue.isBlock.astChildren.sortBy(_.order).l) {
        case (tmpLocal: Local) :: (tmpAssign: Call) :: (xLocal: Local) :: (yLocal: Local) :: (xAssign: Call) :: (yAssign: Call) :: (body: Call) :: Nil =>
          tmpLocal.name shouldBe "<tmp>0"
          tmpAssign.code shouldBe "<tmp>0 = bar()"
          xLocal.name shouldBe "x"
          xLocal.typeFullName shouldBe "i32"
          yLocal.name shouldBe "y"
          yLocal.typeFullName shouldBe "i32"
          xAssign.code shouldBe "x = <tmp>0.x"
          yAssign.code shouldBe "y = <tmp>0.y"
          body.code shouldBe "baz(x, y)"
      }
    }
  }

  "while-let chain" should {
    val cpg = code("""
        |fn main() {
        | while let Some(x) = foo() && x > 0 && let Some(y) = bar(x) {
        |  sink(y);
        | }
        |}
        |""".stripMargin)

    "have correct loop condition" in {
      inside(cpg.whileBlock.condition.isLiteral.l) { case condition :: Nil =>
        condition.code shouldBe "true"
        condition.typeFullName shouldBe "bool"
      }
    }

    "have correct locals" in {
      cpg.whileBlock.whenTrue.isBlock.astChildren.isLocal.name.l shouldBe List("<tmp>0", "x", "<tmp>1", "y")
    }

    "have correct assignments" in {
      cpg.whileBlock.whenTrue.isBlock.astChildren.isCall.code.l shouldBe
        List("<tmp>0 = foo()", "x = <tmp>0.0", "<tmp>1 = bar(x)", "y = <tmp>1.0")
    }

    "have correct condition" in {
      inside(cpg.ifBlock.condition.isCall.l) { case cond :: Nil =>
        cond.methodFullName shouldBe Operators.logicalAnd
        cond.code shouldBe "let Some(x) = foo() && x > 0 && let Some(y) = bar(x)"
      }

      inside(cpg.ifBlock.condition.isCall.argument.sortBy(_.argumentIndex).l) {
        case (lhs: Call) :: (rhs: Unknown) :: Nil =>
          lhs.methodFullName shouldBe Operators.logicalAnd
          lhs.code shouldBe "let Some(x) = foo() && x > 0"
          rhs.code shouldBe "Some(y)"
      }

      inside(cpg.ifBlock.condition.isCall.argument(1).isCall.argument.sortBy(_.argumentIndex).l) {
        case (lhs: Unknown) :: (rhs: Call) :: Nil =>
          lhs.code shouldBe "Some(x)"
          rhs.methodFullName shouldBe Operators.greaterThan
          rhs.code shouldBe "x > 0"
      }
    }

    "have correct then-branch" in {
      inside(cpg.ifBlock.whenTrue.isBlock.astChildren.l) { case (sink: Call) :: Nil =>
        sink.code shouldBe "sink(y)"
      }
    }

    "have correct else-branch" in {
      inside(cpg.ifBlock.whenFalse.isBlock.astChildren.l) { case (break: ControlStructure) :: Nil =>
        break.controlStructureType shouldBe ControlStructureTypes.BREAK
        break.code shouldBe "break"
      }
    }

    "have correct REF edges" in {
      cpg.local.nameExact("x").referencingIdentifiers.lineNumber.l shouldBe List(3, 3, 3)
      cpg.local.nameExact("y").referencingIdentifiers.lineNumber.l shouldBe List(3, 4)
    }
  }

  "while-let chain shadowing previous let" should {
    val cpg = code("""
        |fn main() {
        | let x = 1;
        | while let Some(x) = foo(x) && x > 0 {
        |  bar(x);
        | }
        |}
        |""".stripMargin)

    "have correct locals" in {
      cpg.local.nameExact("x").lineNumber.l shouldBe List(3, 4)
    }

    "have correct REF edges for each local" in {
      cpg.local.nameExact("x").lineNumber(3).referencingIdentifiers.lineNumber.l shouldBe List(3, 4)
      cpg.local.nameExact("x").lineNumber(4).referencingIdentifiers.lineNumber.l shouldBe List(4, 4, 5)
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
