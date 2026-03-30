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
          blockFirstCase.astChildren.code.l should contain allOf ("a", "b", "print(1)")

          jumpTargetSecondCase.label shouldBe NodeTypes.JUMP_TARGET
          jumpTargetSecondCase.code shouldBe "case _ if 5 == 6"

          blockSecondCase.label shouldBe NodeTypes.BLOCK
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

      cpg.call.codeExact("print(1)").head should not be null
      cpg.call.codeExact("print(2)").head should not be null
    }

  }

  "match statement with literal pattern" should {
    val cpg = code("""
        |def someFunc():
        |  match x:
        |    case 42:
        |      print("found")
        |""".stripMargin)

    "have pattern value in case block" in {
      val matchStmt = cpg.controlStructure.head
      val matchBodyBlock = matchStmt.astChildren.order(2).head
      matchBodyBlock.label shouldBe NodeTypes.BLOCK

      matchBodyBlock.astChildren.l match {
        case List(jumpTarget, block) =>
          jumpTarget.label shouldBe NodeTypes.JUMP_TARGET
          jumpTarget.code shouldBe "case 42"

          block.label shouldBe NodeTypes.BLOCK
          block.astChildren.code.l should contain("42")
          block.astChildren.code.l should contain("print(\"found\")")
        case other => fail(s"Expected 2 AST children, but got ${other.size}: $other")
      }
    }
  }

}
