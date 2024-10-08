package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.testfixtures.Py2CpgTestContext
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, nodes}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class WhileCpgTests extends AnyFreeSpec with Matchers {
  "while statement" - {
    lazy val cpg = Py2CpgTestContext.buildCpg("""while x :
        |  y""".stripMargin)

    "test control structure node properties" in {
      val controlStructureNode = cpg.controlStructure.head
      controlStructureNode.code shouldBe "while ... : ..."
      controlStructureNode.controlStructureType shouldBe ControlStructureTypes.WHILE
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

  "while else statement" - {
    lazy val cpg = Py2CpgTestContext.buildCpg("""while x :
        |  y
        |else:
        |  z""".stripMargin)

    "test control structure node properties" in {
      val controlStructureNode = cpg.controlStructure.head
      controlStructureNode.code shouldBe "while ... : ..."
      controlStructureNode.controlStructureType shouldBe ControlStructureTypes.WHILE
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
