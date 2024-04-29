package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class LabelTests extends AstSwiftSrc2CpgSuite {

  "LabelTests" should {

    "testLabel1" in {
      val cpg = code("""
        |loop: while foo() {
        |  if bar { continue loop }
        |  else { break loop }
        |}
        |""".stripMargin)
      val List(jumpTarget) = cpg.method.block.astChildren.astChildren.code("loop:.*").l
      jumpTarget.order shouldBe 1
      val List(jumpTargetDirect) = cpg.jumpTarget.l
      jumpTargetDirect shouldBe jumpTarget
      val List(whileBlock) = cpg.whileBlock.l
      whileBlock.order shouldBe 2
      val List(whileBlockDirect) = cpg.method.block.astChildren.astChildren.code("while.*").l
      whileBlockDirect shouldBe whileBlock

      val List(ifBlock)  = whileBlock.whenTrue.isControlStructure.l
      val List(continue) = ifBlock.whenTrue.isControlStructure.l
      continue.code shouldBe "continue loop"
      continue.controlStructureType shouldBe ControlStructureTypes.CONTINUE
      continue.astChildren.collectAll[JumpLabel].name.l shouldBe List("loop")
      val List(break) = ifBlock.whenFalse.isControlStructure.l
      break.code shouldBe "break loop"
      break.controlStructureType shouldBe ControlStructureTypes.BREAK
      break.astChildren.collectAll[JumpLabel].name.l shouldBe List("loop")
    }

  }

}
