package io.shiftleft.py2cpg.cpg

import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.py2cpg.Py2CpgTestContext
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class UnaryOpCpgTests extends AnyWordSpec with Matchers {
  val testContext = Py2CpgTestContext.newContext.addSource(
    """~1""".stripMargin
  )

  "test unaryOp 'invert' call node properties" in {
    val cpg = testContext.buildCpg

    val plusCall = cpg.call.methodFullName(Operators.not).head
    plusCall.code shouldBe "~1"
    plusCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    plusCall.lineNumber shouldBe Some(1)
    //TODO plusCall.columnNumber shouldBe Some(1)
  }

  "test unaryOp 'invert' ast children" in {
    val cpg = testContext.buildCpg

    cpg.call.methodFullName(Operators.not).astChildren.order(1).isLiteral.head.code shouldBe "1"
  }

  "test unaryOp 'invert' arguments" in {
    val cpg = testContext.buildCpg

    cpg.call.methodFullName(Operators.not).argument.argumentIndex(1).isLiteral.head.code shouldBe "1"
  }
}
