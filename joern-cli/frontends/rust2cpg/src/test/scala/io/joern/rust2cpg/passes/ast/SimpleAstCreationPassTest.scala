package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.semanticcpg.language.*

class SimpleAstCreationPassTest extends Rust2CpgSuite {

  "test 12" in {
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

  "test 13" in {
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

  "test 14" in {
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

  "test 15" in {
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

  "test 16" in {
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

  "test 26" in {
    val cpg = code("""
        |fn main() -> i32 { 1 }
        |""".stripMargin)

    inside(cpg.method.name("main").methodReturn.l) { case (methodRet: MethodReturn) :: Nil =>
      methodRet.typeFullName shouldBe "i32"
      methodRet.code shouldBe "RET"
    }

    inside(cpg.method.name("main").block.astChildren.isReturn.l) { case (ret: Return) :: Nil =>
      ret.code shouldBe "1"

      inside(ret.astChildren.l) { case (lit: Literal) :: Nil =>
        lit.code shouldBe "1"
        lit.typeFullName shouldBe "i32"
      }
    }
  }

  "test 27" in {
    val cpg = code("""
        |fn main() -> char {
        | return 'x';
        |}
        |""".stripMargin)

    cpg.method.name("main").methodReturn.typeFullName.l shouldBe List("char")

    inside(cpg.method.name("main").block.astChildren.l) { case (ret: Return) :: Nil =>
      ret.code shouldBe "return 'x'"

      inside(ret.astChildren.l) { case (lit: Literal) :: Nil =>
        lit.code shouldBe "'x'"
        lit.typeFullName shouldBe "char"
      }
    }
  }

  "test 33" in {
    val cpg = code("""
        |fn main(b: bool) {
        | if !b {
        |  foo();
        | }
        |}
        |""".stripMargin)

    inside(cpg.controlStructure.controlStructureTypeExact(ControlStructureTypes.IF).condition.isCall.l) {
      case (condition: Call) :: Nil =>
        condition.code shouldBe "!b"
        condition.name shouldBe Operators.logicalNot
        condition.methodFullName shouldBe Operators.logicalNot
        condition.typeFullName shouldBe "bool"

        inside(condition.argument.l) { case (b: Identifier) :: Nil =>
          b.typeFullName shouldBe "bool"
          b.code shouldBe "b"
          b.name shouldBe b.code
        }
    }
  }

  "test 34" in {
    val cpg = code("""
        |fn foo() -> i32 {
        | (24)
        |}
        |""".stripMargin)

    inside(cpg.method.name("foo").methodReturn.l) { case (methodRet: MethodReturn) :: Nil =>
      methodRet.typeFullName shouldBe "i32"
      methodRet.code shouldBe "RET"
    }

    inside(cpg.method.name("foo").block.astChildren.isReturn.l) { case (ret: Return) :: Nil =>
      ret.code shouldBe "(24)"

      inside(ret.astChildren.l) { case (lit: Literal) :: Nil =>
        lit.code shouldBe "24"
        lit.typeFullName shouldBe "i32"
      }
    }
  }

}
