package io.joern.pysrc2cpg.cpg

import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.joern.pysrc2cpg.testfixtures.PySrc2CpgFixture

class FormatStringCpgTests extends PySrc2CpgFixture with Matchers {
  "format string" should {
    val cpg = code("""f"pre{x}post"""".stripMargin)

    "test formatString operator node" in {
      val callNode = cpg.call.methodFullName("<operator>.formatString").head
      callNode.code shouldBe """f"pre{x}post""""
      callNode.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      callNode.lineNumber shouldBe Some(1)
    }

    "test formatString operator node arguments" in {
      val callNode = cpg.call.methodFullName("<operator>.formatString").head

      val child1 = callNode.astChildren.order(1).isLiteral.head
      child1.code shouldBe "pre"
      child1.argumentIndex shouldBe 1

      val child2 = callNode.astChildren.order(2).isCall.head
      child2.code shouldBe "{x}"
      child2.argumentIndex shouldBe 2
      child2.methodFullName shouldBe "<operator>.formattedValue"
      child2.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      val child3 = callNode.astChildren.order(3).isLiteral.head
      child3.code shouldBe "post"
      child3.argumentIndex shouldBe 3
    }

    "test formattedValue operator child" in {
      val callNode = cpg.call.methodFullName("<operator>.formattedValue").head

      val child1 = callNode.astChildren.order(1).isIdentifier.head
      child1.code shouldBe "x"
      child1.argumentIndex shouldBe 1
    }
  }

  "test nested format string" in {
    val cpg = code("""
        |f"{f'{a}'} "
        |""".stripMargin)
    val outerCall = cpg.call.methodFullName("<operator>.formatString").columnNumber(1).head
    outerCall.code shouldBe """f"{f'{a}'} """"

    val innerCall = cpg.call.methodFullName("<operator>.formatString").columnNumber(4).head
    innerCall.code shouldBe """f'{a}'"""
  }

  "test format string with multiple replacement fields" in {
    val cpg = code("""
                                           |f"{a} {b}"
                                           |""".stripMargin)
    val callNode = cpg.call.methodFullName("<operator>.formatString").head
    callNode.code shouldBe """f"{a} {b}""""
  }

}
