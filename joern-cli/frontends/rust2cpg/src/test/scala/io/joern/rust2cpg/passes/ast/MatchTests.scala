package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class MatchTests extends Rust2CpgSuite(noSysRoot = true) {

  "a match statement" should {
    val cpg = code("""
        |fn foo(x: i32) {
        |  match x {
        |  1 => one(),
        |  _ => bar(),
        |  };
        |}
        |""".stripMargin)

    "have correct children" in {
      inside(cpg.method.nameExact("foo").block.astChildren.isBlock.astChildren.l) {
        case (matchNode: ControlStructure) :: Nil =>
          matchNode.controlStructureType shouldBe ControlStructureTypes.MATCH
      }
    }

    "have correct match condition" in {
      inside(cpg.controlStructure.controlStructureTypeExact(ControlStructureTypes.MATCH).condition.l) {
        case (ident: Identifier) :: Nil =>
          ident.name shouldBe "x"
          ident.typeFullName shouldBe "i32"
      }
    }

    "have correct REF edges" in {
      cpg.method.nameExact("foo").parameter.referencingIdentifiers.lineNumber.l shouldBe List(3)
    }

    "have jump targets for each match arm" in {
      inside(cpg.controlStructure.controlStructureTypeExact(ControlStructureTypes.MATCH).whenTrue.astChildren.l) {
        case (case1: JumpTarget) :: (arm1: Block) :: (case2: JumpTarget) :: (arm2: Block) :: Nil =>
          case1.name shouldBe "case 1"
          case1.code shouldBe "1"
          inside(arm1.astChildren.l) { case (body: Call) :: Nil =>
            body.name shouldBe "one"
            body.code shouldBe "one()"
          }

          case2.name shouldBe "case _"
          case2.code shouldBe "_"
          inside(arm2.astChildren.l) { case (body: Call) :: Nil =>
            body.name shouldBe "bar"
            body.code shouldBe "bar()"
          }
      }
    }

  }

  "match on the RHS of a let" should {
    val cpg = code("""
        |fn foo(p: (i32, i32)) {
        |  let y = match p {
        |  (a, b) => a + b,
        |  _ => bar(),
        |  };
        |}
        |""".stripMargin)

    "assign the match block to the LHS" in {
      inside(cpg.assignment.where(_.target.isIdentifier.nameExact("y")).source.l) { case (block: Block) :: Nil =>
        inside(block.astChildren.l) { case (matchNode: ControlStructure) :: Nil =>
          matchNode.controlStructureType shouldBe ControlStructureTypes.MATCH
        }
      }
    }

    "have jump targets for each match arm" in {
      inside(cpg.controlStructure.controlStructureTypeExact(ControlStructureTypes.MATCH).whenTrue.astChildren.l) {
        case (case1: JumpTarget) :: (arm1: Block) :: (case2: JumpTarget) :: (arm2: Block) :: Nil =>
          case1.name shouldBe "case (a, b)"
          case1.code shouldBe "(a, b)"
          inside(arm1.astChildren.l) {
            case (aLocal: Local) :: (bLocal: Local) :: (aAssign: Call) :: (bAssign: Call) :: (body: Call) :: Nil =>
              aLocal.name shouldBe "a"
              aLocal.typeFullName shouldBe "i32"
              aAssign.code shouldBe "a = p.0"

              bLocal.name shouldBe "b"
              bLocal.typeFullName shouldBe "i32"
              bAssign.code shouldBe "b = p.1"

              body.code shouldBe "a + b"
              body.methodFullName shouldBe Operators.addition
          }

          case2.name shouldBe "case _"
          case2.code shouldBe "_"
          inside(arm2.astChildren.l) { case (body: Call) :: Nil =>
            body.name shouldBe "bar"
            body.code shouldBe "bar()"
          }
      }
    }
  }

  "match on a call" should {
    val cpg = code("""
        |fn baz() -> i32 { 1 }
        |
        |fn foo() {
        |  match baz() {
        |  1 => one(),
        |  _ => bar(),
        |  };
        |}
        |""".stripMargin)

    "have correct children" in {
      inside(cpg.method.nameExact("foo").block.astChildren.isBlock.astChildren.l) {
        case (tmp: Local) :: (tmpAssign: Call) :: (matchNode: ControlStructure) :: Nil =>
          tmp.name shouldBe "<tmp>0"
          tmp.typeFullName shouldBe "i32"

          tmpAssign.code shouldBe "<tmp>0 = baz()"
          tmpAssign.methodFullName shouldBe Operators.assignment

          matchNode.controlStructureType shouldBe ControlStructureTypes.MATCH
      }
    }

    "have correct match condition" in {
      inside(cpg.controlStructure.controlStructureTypeExact(ControlStructureTypes.MATCH).condition.l) {
        case (tmp: Identifier) :: Nil =>
          tmp.name shouldBe "<tmp>0"
          tmp.typeFullName shouldBe "i32"
      }
    }

    "have jump targets for each match arm" in {
      inside(cpg.controlStructure.controlStructureTypeExact(ControlStructureTypes.MATCH).whenTrue.astChildren.l) {
        case (case1: JumpTarget) :: (arm1: Block) :: (case2: JumpTarget) :: (arm2: Block) :: Nil =>
          case1.name shouldBe "case 1"
          case1.code shouldBe "1"
          inside(arm1.astChildren.l) { case (body: Call) :: Nil =>
            body.name shouldBe "one"
            body.code shouldBe "one()"
          }

          case2.name shouldBe "case _"
          case2.code shouldBe "_"
          inside(arm2.astChildren.l) { case (body: Call) :: Nil =>
            body.name shouldBe "bar"
            body.code shouldBe "bar()"
          }
      }
    }
  }

  "match with guards" should {
    val cpg = code("""
        |fn foo(x: (i32, i32)) {
        |  match x {
        |  (n, _) if n > 3 => bar(n),
        |  _ if baz() => qux(),
        |  _ => 0,
        |  };
        |}
        |""".stripMargin)

    "have correct jump target names" in {
      cpg.jumpTarget.sortBy(_.order).name.l shouldBe List("case (n, _) if n > 3", "case _ if baz()", "case _")
    }

    "have correct locals" in {
      inside(cpg.local.sortBy(_.order).l) { case nLocal :: Nil =>
        nLocal.name shouldBe "n"
        nLocal.typeFullName shouldBe "i32"
      }
    }

    "have correct local assignments" in {
      cpg.method.nameExact("foo").block.ast.isCall.isAssignment.sortBy(_.order).code.l shouldBe List("n = x.0")
    }

    "have correct if control structures" in {
      inside(cpg.ifBlock.sortBy(_.lineNumber).l) { case guard1 :: guard2 :: Nil =>
        guard1.code shouldBe "if n > 3"
        guard1.condition.code.l shouldBe List("n > 3")
        guard1.whenTrue.code.l shouldBe List("bar(n)")
        guard1.whenFalse shouldBe empty

        guard2.code shouldBe "if baz()"
        guard2.condition.code.l shouldBe List("baz()")
        guard2.whenTrue.code.l shouldBe List("qux()")
        guard2.whenFalse shouldBe empty
      }
    }
  }

  "match with an or-pattern case" should {
    val cpg = code("""
        |struct Point { x: i32, y: i32 }
        |
        |fn foo(p: Point) {
        |  match p {
        |  Point { x: 0, y: a } | Point { x: a, y: 0 } => bar(a),
        |  _ => 0,
        |  };
        |}
        |""".stripMargin)

    "have correct locals" in {
      inside(cpg.local.l) { case aLocal :: tmp :: Nil =>
        aLocal.name shouldBe "a"
        aLocal.typeFullName shouldBe "i32"
        tmp.name shouldBe "<tmp>0"
        tmp.typeFullName shouldBe "rust2cpgtest::Point"
      }
    }

    "have correct assignments" in {
      cpg.method.nameExact("foo").block.ast.isCall.isAssignment.code.sorted.l shouldBe
        List("<tmp>0 = p", "a = <tmp>0.x", "a = <tmp>0.y")
    }

    "have correct if control structure" in {
      inside(cpg.ifBlock.l) { case ifNode :: elseIfNode :: Nil =>
        ifNode.condition.code.l shouldBe List("Point { x: 0, y: a }")
        ifNode.whenTrue.isBlock.astChildren.isCall.isAssignment.code.l shouldBe List("a = <tmp>0.y")
        ifNode.whenFalse.l shouldBe List(elseIfNode)

        elseIfNode.condition.code.l shouldBe List("Point { x: a, y: 0 }")
        elseIfNode.whenTrue.isBlock.astChildren.isCall.isAssignment.code.l shouldBe List("a = <tmp>0.x")
        elseIfNode.whenFalse.l shouldBe empty
      }
    }
  }

  "match with a unit variant case" should {
    val cpg = code("""
        |enum E { A(i32), B }
        |use E::*;
        |
        |fn foo(e: E) -> i32 {
        |  match e {
        |    A(x) => x,
        |    B => 0,
        |  }
        |}
        |""".stripMargin)

    "have correct locals" in {
      inside(cpg.method.nameExact("foo").local.l) { case xLocal :: Nil =>
        xLocal.name shouldBe "x"
        xLocal.typeFullName shouldBe "i32"
      }
    }

    "have correct assignments" in {
      cpg.method.nameExact("foo").call.isAssignment.code.l shouldBe
        List("x = (e as rust2cpgtest::E::A).0")
    }
  }

  "match with a const case" should {
    val cpg = code("""
        |const MAX: i32 = 10;
        |
        |fn foo(n: i32) -> i32 {
        |  match n {
        |    MAX => 1,
        |    y => y,
        |  }
        |}
        |""".stripMargin)

    "have correct locals" in {
      inside(cpg.method.nameExact("foo").local.l) { case yLocal :: Nil =>
        yLocal.name shouldBe "y"
        yLocal.typeFullName shouldBe "i32"
      }
    }

    "have correct assignments" in {
      cpg.method.nameExact("foo").call.isAssignment.code.l shouldBe List("y = n")
    }
  }

  "match with slice pattern case" should {
    val cpg = code("""
        |fn foo(s: &[i32]) -> i32 {
        |  match s {
        |    [] => 0,
        |    [first, ..] => *first,
        |  }
        |}
        |""".stripMargin)

    "have correct locals" in {
      inside(cpg.method.nameExact("foo").local.l) { case first :: Nil =>
        first.name shouldBe "first"
        first.typeFullName shouldBe "&i32"
      }
    }

    "have correct assignments" in {
      cpg.method.nameExact("foo").call.isAssignment.code.l shouldBe List("first = s[0]")
    }
  }

  "match with range case" should {
    val cpg = code("""
        |fn foo(n: i32) -> i32 {
        |  match n {
        |    1..=5 => 1,
        |    y => y,
        |  }
        |}
        |""".stripMargin)

    "have correct jump targets" in {
      inside(cpg.controlStructure.controlStructureTypeExact(ControlStructureTypes.MATCH).whenTrue.astChildren.l) {
        case (case1: JumpTarget) :: (block1: Block) :: (case2: JumpTarget) :: (block2: Block) :: Nil =>
          case1.name shouldBe "case 1..=5"
          case1.code shouldBe "1..=5"
          inside(block1.astChildren.l) { case (lit: Literal) :: Nil =>
            lit.typeFullName shouldBe "i32"
            lit.code shouldBe "1"
          }

          case2.name shouldBe "case y"
          case2.code shouldBe "y"
          inside(block2.astChildren.l) { case (yLocal: Local) :: (assignment: Call) :: (yIdent: Identifier) :: Nil =>
            yLocal.name shouldBe "y"
            yLocal.typeFullName shouldBe "i32"
            assignment.code shouldBe "y = n"
            yIdent.name shouldBe "y"
            yIdent.typeFullName shouldBe "i32"
          }
      }
    }
  }

  "match shadowing previous let" should {
    val cpg = code("""
        |fn foo(opt: Option<i32>) {
        |  let y = 1;
        |  match opt {
        |  Some(y) => bar(y),
        |  _ => bar(y),
        |  };
        |  bar(y);
        |}
        |""".stripMargin)

    "have correct locals" in {
      cpg.local.nameExact("y").lineNumber.l shouldBe List(3, 5)
    }

    "have correct REF edges for each local" in {
      cpg.local.nameExact("y").lineNumber(3).referencingIdentifiers.lineNumber.l shouldBe List(3, 6, 8)
      cpg.local.nameExact("y").lineNumber(5).referencingIdentifiers.lineNumber.l shouldBe List(5, 5)
    }
  }

}
