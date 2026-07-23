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

    "lower as a block with a local, tmp assignment and MATCH" in {
      inside(cpg.method.nameExact("foo").block.astChildren.isBlock.astChildren.l) {
        case (tmp: Local) :: (tmpAssign: Call) :: (matchNode: ControlStructure) :: Nil =>
          tmp.name shouldBe "tmp"
          tmp.typeFullName shouldBe "i32"

          tmpAssign.code shouldBe "tmp = x"
          tmpAssign.methodFullName shouldBe Operators.assignment

          matchNode.controlStructureType shouldBe ControlStructureTypes.MATCH
      }
    }

    "have correct match condition" in {
      inside(cpg.controlStructure.controlStructureTypeExact(ControlStructureTypes.MATCH).condition.l) {
        case (tmp: Identifier) :: Nil =>
          tmp.name shouldBe "tmp"
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
        inside(block.astChildren.l) { case (tmp: Local) :: (tmpAssign: Call) :: (matchNode: ControlStructure) :: Nil =>
          tmp.name shouldBe "tmp"
          tmp.typeFullName shouldBe "(i32, i32)"

          tmpAssign.code shouldBe "tmp = p"
          tmpAssign.methodFullName shouldBe Operators.assignment

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
              aAssign.code shouldBe "a = tmp.0"

              bLocal.name shouldBe "b"
              bLocal.typeFullName shouldBe "i32"
              bAssign.code shouldBe "b = tmp.1"

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

}
