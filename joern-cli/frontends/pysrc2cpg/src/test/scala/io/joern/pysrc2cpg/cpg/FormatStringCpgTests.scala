package io.joern.pysrc2cpg.cpg

import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.joern.pysrc2cpg.Py2CpgTestContext
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class FormatStringCpgTests extends AnyFreeSpec with Matchers {
  "format string" - {
    lazy val cpg = Py2CpgTestContext.buildCpg("""f"pre{x}post"""".stripMargin)

    "test formatString operator node" in {
      val callNode = cpg.call.methodFullName("<operator>.formatString").next()
      callNode.code shouldBe """f"pre{x}post""""
      callNode.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      callNode.lineNumber shouldBe Some(1)
    }

    "test formatString operator node arguments" in {
      val callNode = cpg.call.methodFullName("<operator>.formatString").next()

      val child1 = callNode.astChildren.order(1).isLiteral.next()
      child1.code shouldBe "pre"
      child1.argumentIndex shouldBe 1

      val child2 = callNode.astChildren.order(2).isCall.next()
      child2.code shouldBe "{x}"
      child2.argumentIndex shouldBe 2
      child2.methodFullName shouldBe "<operator>.formattedValue"
      child2.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      val child3 = callNode.astChildren.order(3).isLiteral.next()
      child3.code shouldBe "post"
      child3.argumentIndex shouldBe 3
    }

    "test formattedValue operator child" in {
      val callNode = cpg.call.methodFullName("<operator>.formattedValue").next()

      val child1 = callNode.astChildren.order(1).isIdentifier.next()
      child1.code shouldBe "x"
      child1.argumentIndex shouldBe 1
    }
  }

  "test nested format string" in {
    val cpg = Py2CpgTestContext.buildCpg("""
        |f"{f'{a}'} "
        |""".stripMargin)
    val outerCall = cpg.call.methodFullName("<operator>.formatString").columnNumber(1).next()
    outerCall.code shouldBe """f"{f'{a}'} """"

    val innerCall = cpg.call.methodFullName("<operator>.formatString").columnNumber(4).next()
    innerCall.code shouldBe """f'{a}'"""
  }

  "test format string with multiple replacement fields" in {
    val cpg = Py2CpgTestContext.buildCpg("""
                                           |f"{a} {b}"
                                           |""".stripMargin)
    val callNode = cpg.call.methodFullName("<operator>.formatString").next()
    callNode.code shouldBe """f"{a} {b}""""
  }

}
