package io.joern.pysrc2cpg.cpg

import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.semanticcpg.language.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.joern.pysrc2cpg.testfixtures.PySrc2CpgFixture

class BinOpCpgTests extends PySrc2CpgFixture with Matchers {
  val cpg = code("""1 + 2""".stripMargin)

  "test binOp 'add' call node properties" in {
    val additionCall = cpg.call.methodFullName(Operators.addition).head
    additionCall.code shouldBe "1 + 2"
    additionCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    additionCall.lineNumber shouldBe Some(1)
    additionCall.columnNumber shouldBe Some(1)
  }

  "test 1 literal node properties" in {
    val literal = cpg.literal("1").head
    literal.code shouldBe "1"
    literal.lineNumber shouldBe Some(1)
    literal.columnNumber shouldBe Some(1)
  }

  "test 2 literal node properties" in {
    val literal = cpg.literal("2").head
    literal.code shouldBe "2"
    literal.lineNumber shouldBe Some(1)
    literal.columnNumber shouldBe Some(5)
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
