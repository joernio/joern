package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.testfixtures.Py2CpgTestContext
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class RaiseCpgTests extends AnyFreeSpec with Matchers {
  "python3 raise" - {
    lazy val cpg = Py2CpgTestContext.buildCpg("""raise x from y
        |""".stripMargin)

    "test raise node properties" in {
      val raiseNode = cpg.call.methodFullName("<operator>.raise").next()
      raiseNode.code shouldBe "raise x from y"
      raiseNode.lineNumber shouldBe Some(1)
    }

    "test raise node ast children" in {
      cpg.call
        .methodFullName("<operator>.raise")
        .astChildren
        .order(1)
        .isIdentifier
        .head
        .code shouldBe "x"
      cpg.call
        .methodFullName("<operator>.raise")
        .astChildren
        .order(2)
        .isIdentifier
        .head
        .code shouldBe "y"
    }

    "test raise node arguments" in {
      cpg.call
        .methodFullName("<operator>.raise")
        .argument
        .argumentIndex(1)
        .isIdentifier
        .head
        .code shouldBe "x"
      cpg.call
        .methodFullName("<operator>.raise")
        .argument
        .argumentIndex(2)
        .isIdentifier
        .head
        .code shouldBe "y"
    }
  }
}
