package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.testfixtures.PySrc2CpgFixture
import io.shiftleft.codepropertygraph.generated.{NodeTypes, nodes}
import io.shiftleft.semanticcpg.language.*

class MatchCpgTests extends PySrc2CpgFixture() {
  "match statement with guards" should {
    val cpg = code("""
        |def someFunc():
        |  match [1, 2]:
        |    case [a, b] if 3 == 4:
        |      print(1)
        |    case _ if 5 == 6:
        |      print(2)
        |    case _:
        |      print(3)
        |""".stripMargin)

    "have correct AST" in {
      val matchStmt = cpg.controlStructure.head
      val condition = matchStmt.astChildren.order(1).head
      condition.label shouldBe NodeTypes.CALL
      condition.code shouldBe "[1, 2]"
      condition.lineNumber shouldBe Some(3)

      val matchBodyBlock = matchStmt.astChildren.order(2).head
      matchBodyBlock.label shouldBe NodeTypes.BLOCK

      matchBodyBlock.astChildren.l match {
        case List(
              jumpTargetFirstCase,
              blockFirstCase,
              jumpTargetSecondCase,
              blockSecondCase,
              jumpTargetThirdCase,
              blockThirdCase
            ) =>
          jumpTargetFirstCase.label shouldBe NodeTypes.JUMP_TARGET
          jumpTargetFirstCase.code shouldBe "case [a, b] if 3 == 4"

          blockFirstCase.label shouldBe NodeTypes.BLOCK
          // Block now contains pattern nodes (a, b), guard (3 == 4), and body (print(1))
          blockFirstCase.astChildren.code.l should contain allOf ("a", "b", "print(1)")

          jumpTargetSecondCase.label shouldBe NodeTypes.JUMP_TARGET
          jumpTargetSecondCase.code shouldBe "case _ if 5 == 6"

          blockSecondCase.label shouldBe NodeTypes.BLOCK
          // Block now contains guard (5 == 6) and body (print(2))
          blockSecondCase.astChildren.code.l should contain("print(2)")

          jumpTargetThirdCase.label shouldBe NodeTypes.JUMP_TARGET
          jumpTargetThirdCase.code shouldBe "default"

          blockThirdCase.label shouldBe NodeTypes.BLOCK
          blockThirdCase.code shouldBe "print(3)"
        case other => fail(s"Expected 6 AST children, but got ${other.size}: $other")
      }
    }
  }

  "match statement" should {
    val cpg = code("""
        |def someFunc():
        |  match [1, 2]:
        |    case [a, b]:
        |      print(1)
        |    case _:
        |      print(2)
        |""".stripMargin)

    "have correct AST" in {
      val matchStmt = cpg.controlStructure.head
      val condition = matchStmt.astChildren.order(1).head
      condition.label shouldBe NodeTypes.CALL
      condition.code shouldBe "[1, 2]"
      condition.lineNumber shouldBe Some(3)

      val matchBodyBlock = matchStmt.astChildren.order(2).head
      matchBodyBlock.label shouldBe NodeTypes.BLOCK

      matchBodyBlock.astChildren.l match {
        case List(jumpTargetFirstCase, blockFirstCase, jumpTargetSecondCase, blockSecondCase) =>
          jumpTargetFirstCase.label shouldBe NodeTypes.JUMP_TARGET
          jumpTargetFirstCase.code shouldBe "case [a, b]"

          blockFirstCase.label shouldBe NodeTypes.BLOCK
          // Block now contains pattern nodes (a, b) and body (print(1))
          blockFirstCase.astChildren.code.l should contain allOf ("a", "b", "print(1)")

          jumpTargetSecondCase.label shouldBe NodeTypes.JUMP_TARGET
          jumpTargetSecondCase.code shouldBe "default"

          blockSecondCase.label shouldBe NodeTypes.BLOCK
          blockSecondCase.code shouldBe "print(2)"
        case other => fail(s"Expected 4 AST children, but got ${other.size}: $other")
      }
    }

    "have correct CFG" in {
      val methodNode = cpg.method.nameExact("someFunc").head
      methodNode.cfgOut.l match {
        case List(firstConditionExpr) =>
          firstConditionExpr.code shouldBe "1"
        case other => fail(s"Expected 1 CFG successor, but got ${other.size}: $other")
      }

      val conditionCall = cpg.call.codeExact("[1, 2]").head
      conditionCall.cfgOut.l match {
        case List(jumpTargetFirstCase, jumpTargetSecondCase) =>
          jumpTargetFirstCase.label shouldBe NodeTypes.JUMP_TARGET
          jumpTargetFirstCase.code shouldBe "case [a, b]"

          jumpTargetSecondCase.label shouldBe NodeTypes.JUMP_TARGET
          jumpTargetSecondCase.code shouldBe "default"
        case other => fail(s"Expected 2 CFG successors, but got ${other.size}: $other")
      }

      // The first case block now includes pattern identifiers, so find it via the print(1) call.
      val print1Block = cpg.call.codeExact("print(1)").astParent.isBlock.head
      print1Block.cfgOut.l match {
        case List(methodReturn) =>
          methodReturn.label shouldBe NodeTypes.METHOD_RETURN
        case other => fail(s"Expected 1 CFG successor, but got ${other.size}: $other")
      }

      val print2Block = cpg.block.codeExact("print(2)").head
      print2Block.cfgOut.l match {
        case List(methodReturn) =>
          methodReturn.label shouldBe NodeTypes.METHOD_RETURN
        case other => fail(s"Expected 1 CFG successor, but got ${other.size}: $other")
      }

    }

  }

  "match statement with literal value pattern" should {
    val cpg = code("""
        |def someFunc():
        |  match x:
        |    case 42:
        |      print("found")
        |    case _:
        |      print("default")
        |""".stripMargin)

    "have pattern value as AST child of case block" in {
      val matchStmt      = cpg.controlStructure.head
      val matchBodyBlock = matchStmt.astChildren.order(2).head

      matchBodyBlock.astChildren.l match {
        case List(jumpTargetFirst, blockFirst, jumpTargetSecond, blockSecond) =>
          jumpTargetFirst.label shouldBe NodeTypes.JUMP_TARGET
          jumpTargetFirst.code shouldBe "case 42"

          blockFirst.label shouldBe NodeTypes.BLOCK
          // Block contains the literal 42 pattern node and the body
          blockFirst.astChildren.code.l should contain("42")
          blockFirst.astChildren.code.l should contain("print(\"found\")")

          jumpTargetSecond.label shouldBe NodeTypes.JUMP_TARGET
          jumpTargetSecond.code shouldBe "default"

          blockSecond.label shouldBe NodeTypes.BLOCK
          blockSecond.code shouldBe "print(\"default\")"
        case other => fail(s"Expected 4 AST children, but got ${other.size}: $other")
      }
    }
  }

  "match statement default case" should {
    val cpg = code("""
        |def someFunc():
        |  match x:
        |    case 1:
        |      print("one")
        |    case _:
        |      print("other")
        |""".stripMargin)

    "have default jump target with no pattern nodes in default block" in {
      val matchStmt      = cpg.controlStructure.head
      val matchBodyBlock = matchStmt.astChildren.order(2).head

      matchBodyBlock.astChildren.l match {
        case List(jumpTargetFirst, _, jumpTargetSecond, blockSecond) =>
          jumpTargetSecond.label shouldBe NodeTypes.JUMP_TARGET
          jumpTargetSecond.code shouldBe "default"

          blockSecond.label shouldBe NodeTypes.BLOCK
          // Default block should only contain the body, no pattern nodes
          blockSecond.astChildren.l.size shouldBe 1
          blockSecond.code shouldBe "print(\"other\")"
        case other => fail(s"Expected 4 AST children, but got ${other.size}: $other")
      }
    }
  }

}
