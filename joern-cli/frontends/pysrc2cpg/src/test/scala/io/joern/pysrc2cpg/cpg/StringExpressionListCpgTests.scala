package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.testfixtures.Py2CpgTestContext
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class StringExpressionListCpgTests extends AnyFreeSpec with Matchers {
  "string expression list" - {
    lazy val cpg = Py2CpgTestContext.buildCpg(""""one" "two" "three"""".stripMargin)

    "test stringExpressionList operator node" in {
      val callNode = cpg.call.methodFullName("<operator>.stringExpressionList").head
      callNode.code shouldBe """"one" "two" "three""""
      callNode.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      callNode.lineNumber shouldBe Some(1)
    }

    "test stringExpressionList operator node arguments" in {
      val callNode = cpg.call.methodFullName("<operator>.stringExpressionList").head

      val child1 = callNode.astChildren.order(1).isLiteral.head
      child1.code shouldBe "\"one\""
      child1.argumentIndex shouldBe 1

      val child2 = callNode.astChildren.order(2).isLiteral.head
      child2.code shouldBe "\"two\""
      child2.argumentIndex shouldBe 2

      val child3 = callNode.astChildren.order(3).isLiteral.head
      child3.code shouldBe "\"three\""
      child3.argumentIndex shouldBe 3
    }
  }

}
