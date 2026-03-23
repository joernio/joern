package io.joern.pysrc2cpg.cpg

import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, nodes}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.joern.pysrc2cpg.testfixtures.PySrc2CpgFixture

class IfCpgTests extends PySrc2CpgFixture with Matchers {
  "if statement" should {
    val cpg = code("""if x :
        |  y""".stripMargin)

    "test control structure node properties" in {
      val controlStructureNode = cpg.controlStructure.head
      controlStructureNode.code shouldBe "if ... : ..."
      controlStructureNode.controlStructureType shouldBe ControlStructureTypes.IF
      controlStructureNode.lineNumber shouldBe Some(1)
    }

    "test control structure condition" in {
      val conditionNode = cpg.controlStructure.condition.head
      conditionNode.code shouldBe "x"
      cpg.controlStructure.astChildren.order(1).head shouldBe conditionNode
    }

    "test control structure body" in {
      val bodyNode = cpg.controlStructure.whenTrue.isExpression.head
      bodyNode.isInstanceOf[nodes.Block] shouldBe true
      bodyNode.code shouldBe "y"
      cpg.controlStructure.astChildren.order(2).head shouldBe bodyNode
    }
  }

  "if else statement" should {
    val cpg = code("""if x :
        |  y
        |else:
        |  z""".stripMargin)

    "test control structure node properties" in {
      val controlStructureNode = cpg.controlStructure.head
      controlStructureNode.code shouldBe "if ... : ..."
      controlStructureNode.controlStructureType shouldBe ControlStructureTypes.IF
      controlStructureNode.lineNumber shouldBe Some(1)
    }

    "test control structure condition" in {
      val conditionNode = cpg.controlStructure.condition.head
      conditionNode.code shouldBe "x"
      cpg.controlStructure.astChildren.order(1).head shouldBe conditionNode
    }

    "test control structure body" in {
      val bodyNode = cpg.controlStructure.whenTrue.isExpression.head
      bodyNode.isInstanceOf[nodes.Block] shouldBe true
      bodyNode.code shouldBe "y"
      cpg.controlStructure.astChildren.order(2).head shouldBe bodyNode
    }

    "test control structure else" in {
      val elseNode = cpg.controlStructure.whenFalse.isExpression.head
      elseNode.isInstanceOf[nodes.Block] shouldBe true
      elseNode.code shouldBe "z"
      cpg.controlStructure.astChildren.order(3).head shouldBe elseNode
    }
  }
}
