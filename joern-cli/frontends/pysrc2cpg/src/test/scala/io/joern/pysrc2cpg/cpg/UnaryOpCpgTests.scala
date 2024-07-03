package io.joern.pysrc2cpg.cpg

import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.joern.pysrc2cpg.Py2CpgTestContext
import io.shiftleft.semanticcpg.language.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class UnaryOpCpgTests extends AnyFreeSpec with Matchers {
  lazy val cpg = Py2CpgTestContext.buildCpg("""~1""".stripMargin)

  "test unaryOp 'invert' call node properties" in {
    val plusCall = cpg.call.methodFullName(Operators.not).head
    plusCall.code shouldBe "~1"
    plusCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    plusCall.lineNumber shouldBe Some(1)
    // TODO plusCall.columnNumber shouldBe Some(1)
  }

  "test unaryOp 'invert' ast children" in {
    cpg.call.methodFullName(Operators.not).astChildren.order(1).isLiteral.head.code shouldBe "1"
  }

  "test unaryOp 'invert' arguments" in {
    cpg.call
      .methodFullName(Operators.not)
      .argument
      .argumentIndex(1)
      .isLiteral
      .head
      .code shouldBe "1"
  }
}
