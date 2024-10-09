package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.testfixtures.Py2CpgTestContext
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators, nodes}
import io.shiftleft.semanticcpg.language.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class AssignCpgTests extends AnyFreeSpec with Matchers {
  "single target assign" - {
    lazy val cpg = Py2CpgTestContext.buildCpg("""x = 2""".stripMargin)

    "test assignment node properties" in {
      val assignCall = cpg.call.methodFullName(Operators.assignment).head
      assignCall.code shouldBe "x = 2"
      assignCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      assignCall.lineNumber shouldBe Some(1)
      assignCall.columnNumber shouldBe Some(1)
    }

    "test assignment node ast children" in {
      cpg.call
        .methodFullName(Operators.assignment)
        .astChildren
        .order(1)
        .isIdentifier
        .head
        .code shouldBe "x"
      cpg.call
        .methodFullName(Operators.assignment)
        .astChildren
        .order(2)
        .isLiteral
        .head
        .code shouldBe "2"
    }

    "test assignment node arguments" in {
      cpg.call
        .methodFullName(Operators.assignment)
        .argument
        .argumentIndex(1)
        .isIdentifier
        .head
        .code shouldBe "x"
      cpg.call
        .methodFullName(Operators.assignment)
        .argument
        .argumentIndex(2)
        .isLiteral
        .head
        .code shouldBe "2"
    }
  }

  "nested decomposing assign" - {
    // Nested decomposing assign statements get lowered to a block with
    // a local variable for the right hand side and an assignment
    // inside the block for each element in the target list.
    lazy val cpg = Py2CpgTestContext.buildCpg("""x, (y, z) = list""".stripMargin)

    def getSurroundingBlock: nodes.Block = {
      cpg.all.collect { case block: nodes.Block if block.code != "" => block }.head
    }

    "test block exists" in {
      // Throws if block does not exist.
      getSurroundingBlock
    }

    "test block node properties" in {
      val block = getSurroundingBlock
      block.code shouldBe
        """tmp0 = list
          |x = tmp0[0]
          |y = tmp0[1][0]
          |z = tmp0[1][1]""".stripMargin
      block.lineNumber shouldBe Some(1)
    }

    "test local node" in {
      cpg.method.name("<module>").local.name("tmp0").headOption should not be empty
    }

    "test tmp variable assignment" in {
      val block         = getSurroundingBlock
      val tmpAssignNode = block.astChildren.isCall.sortBy(_.order).head
      tmpAssignNode.code shouldBe "tmp0 = list"
      tmpAssignNode.methodFullName shouldBe Operators.assignment
      tmpAssignNode.lineNumber shouldBe Some(1)
    }

    "test assignments to targets" in {
      val block       = getSurroundingBlock
      val assignNodes = block.astChildren.isCall.sortBy(_.order).tail
      assignNodes.map(_.code) should contain theSameElementsInOrderAs List(
        "x = tmp0[0]",
        "y = tmp0[1][0]",
        "z = tmp0[1][1]"
      )
      assignNodes.map(_.lineNumber.get) should contain theSameElementsInOrderAs List(1, 1, 1)
    }
  }

  "multi target assign" - {
    // Multi target assign statements get lowered to a block with
    // a local variable for the right hand side and an assignment
    // inside the block for each element in the target list.
    lazy val cpg = Py2CpgTestContext.buildCpg("""x = y = list""".stripMargin)

    def getSurroundingBlock: nodes.Block = {
      cpg.all.collect { case block: nodes.Block if block.code != "" => block }.head
    }

    "test block exists" in {
      // Throws if block does not exist.
      getSurroundingBlock
    }

    "test block node properties" in {
      val block = getSurroundingBlock
      block.code shouldBe
        """tmp0 = list
          |x = tmp0
          |y = tmp0""".stripMargin
      block.lineNumber shouldBe Some(1)
    }

    "test local node" in {
      cpg.method.name("<module>").local.name("tmp0").headOption should not be empty
    }

    "test tmp variable assignment" in {
      val block         = getSurroundingBlock
      val tmpAssignNode = block.astChildren.isCall.sortBy(_.order).head
      tmpAssignNode.code shouldBe "tmp0 = list"
      tmpAssignNode.methodFullName shouldBe Operators.assignment
      tmpAssignNode.lineNumber shouldBe Some(1)
    }

    "test assignments to targets" in {
      val block       = getSurroundingBlock
      val assignNodes = block.astChildren.isCall.sortBy(_.order).tail
      assignNodes.map(_.code) should contain theSameElementsInOrderAs List("x = tmp0", "y = tmp0")
      assignNodes.map(_.lineNumber.get) should contain theSameElementsInOrderAs List(1, 1)
    }
  }

  "annotated assign" - {
    lazy val cpg = Py2CpgTestContext.buildCpg("""x: y = z""".stripMargin)

    "test assignment node properties" in {
      val assignCall = cpg.call.methodFullName(Operators.assignment).head
      assignCall.code shouldBe "x = z"
      assignCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      assignCall.lineNumber shouldBe Some(1)
      assignCall.columnNumber shouldBe Some(1)
    }

    "test assignment node ast children" in {
      cpg.call
        .methodFullName(Operators.assignment)
        .astChildren
        .order(1)
        .isIdentifier
        .head
        .code shouldBe "x"
      cpg.call
        .methodFullName(Operators.assignment)
        .astChildren
        .order(2)
        .isIdentifier
        .head
        .code shouldBe "z"
    }

    "test assignment node arguments" in {
      cpg.call
        .methodFullName(Operators.assignment)
        .argument
        .argumentIndex(1)
        .isIdentifier
        .head
        .code shouldBe "x"
      cpg.call
        .methodFullName(Operators.assignment)
        .argument
        .argumentIndex(2)
        .isIdentifier
        .head
        .code shouldBe "z"
    }
  }

  "annotated assign without value" - {
    lazy val cpg = Py2CpgTestContext.buildCpg("""x: y""".stripMargin)

    "test target expression node properties" in {
      val assignCall = cpg.identifier.name("x").head
      assignCall.code shouldBe "x"
      assignCall.lineNumber shouldBe Some(1)
      assignCall.columnNumber shouldBe Some(1)
    }
  }

  "augmented assign" - {
    lazy val cpg = Py2CpgTestContext.buildCpg("""x += y""".stripMargin)

    "test assignment node properties" in {
      val assignCall = cpg.call.methodFullName(Operators.assignmentPlus).head
      assignCall.code shouldBe "x += y"
      assignCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      assignCall.lineNumber shouldBe Some(1)
      assignCall.columnNumber shouldBe Some(1)
    }

    "test assignment node ast children" in {
      cpg.call
        .methodFullName(Operators.assignmentPlus)
        .astChildren
        .order(1)
        .isIdentifier
        .head
        .code shouldBe "x"
      cpg.call
        .methodFullName(Operators.assignmentPlus)
        .astChildren
        .order(2)
        .isIdentifier
        .head
        .code shouldBe "y"
    }

    "test assignment node arguments" in {
      cpg.call
        .methodFullName(Operators.assignmentPlus)
        .argument
        .argumentIndex(1)
        .isIdentifier
        .head
        .code shouldBe "x"
      cpg.call
        .methodFullName(Operators.assignmentPlus)
        .argument
        .argumentIndex(2)
        .isIdentifier
        .head
        .code shouldBe "y"
    }
  }
}
