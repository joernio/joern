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
          blockFirstCase.code shouldBe "print(1)"

          jumpTargetSecondCase.label shouldBe NodeTypes.JUMP_TARGET
          jumpTargetSecondCase.code shouldBe "case _ if 5 == 6"

          blockSecondCase.label shouldBe NodeTypes.BLOCK
          blockSecondCase.code shouldBe "print(2)"

          jumpTargetThirdCase.label shouldBe NodeTypes.JUMP_TARGET
          jumpTargetThirdCase.code shouldBe "default"

          blockThirdCase.label shouldBe NodeTypes.BLOCK
          blockThirdCase.code shouldBe "print(3)"
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
          blockFirstCase.code shouldBe "print(1)"

          jumpTargetSecondCase.label shouldBe NodeTypes.JUMP_TARGET
          jumpTargetSecondCase.code shouldBe "default"

          blockSecondCase.label shouldBe NodeTypes.BLOCK
          blockSecondCase.code shouldBe "print(2)"
      }
    }

    "have correct CFG" in {
      val methodNode = cpg.method.nameExact("someFunc").head
      methodNode.cfgOut.l match {
        case List(firstConditionExpr) =>
          firstConditionExpr.code shouldBe "1"
      }

      val conditionCall = cpg.call.codeExact("[1, 2]").head
      conditionCall.cfgOut.l match {
        case List(jumpTargetFirstCase, jumpTargetSecondCase) =>
          jumpTargetFirstCase.label shouldBe NodeTypes.JUMP_TARGET
          jumpTargetFirstCase.code shouldBe "case [a, b]"

          jumpTargetSecondCase.label shouldBe NodeTypes.JUMP_TARGET
          jumpTargetSecondCase.code shouldBe "default"
      }

      val print1Block = cpg.block.codeExact("print(1)").head
      print1Block.cfgOut.l match {
        case List(methodReturn) =>
          methodReturn.label shouldBe NodeTypes.METHOD_RETURN
      }

      val print2Block = cpg.block.codeExact("print(2)").head
      print2Block.cfgOut.l match {
        case List(methodReturn) =>
          methodReturn.label shouldBe NodeTypes.METHOD_RETURN
      }

    }

  }

}
