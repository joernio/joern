package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.testfixtures.Py2CpgTestContext
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.semanticcpg.language.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class BinOpCpgTests extends AnyFreeSpec with Matchers {
  lazy val cpg = Py2CpgTestContext.buildCpg("""1 + 2""".stripMargin)

  "test binOp 'add' call node properties" in {
    val additionCall = cpg.call.methodFullName(Operators.addition).head
    additionCall.code shouldBe "1 + 2"
    additionCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    additionCall.lineNumber shouldBe Some(1)
    // TODO additionCall.columnNumber shouldBe Some(1)
  }

  "test binOp 'add' ast children" in {
    cpg.call
      .methodFullName(Operators.addition)
      .astChildren
      .order(1)
      .isLiteral
      .head
      .code shouldBe "1"
    cpg.call
      .methodFullName(Operators.addition)
      .astChildren
      .order(2)
      .isLiteral
      .head
      .code shouldBe "2"
  }

  "test binOp 'add' arguments" in {
    cpg.call
      .methodFullName(Operators.addition)
      .argument
      .argumentIndex(1)
      .isLiteral
      .head
      .code shouldBe "1"
    cpg.call
      .methodFullName(Operators.addition)
      .argument
      .argumentIndex(2)
      .isLiteral
      .head
      .code shouldBe "2"
  }
}
