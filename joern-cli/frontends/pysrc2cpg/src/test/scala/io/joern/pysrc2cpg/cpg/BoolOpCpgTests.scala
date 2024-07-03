package io.joern.pysrc2cpg.cpg

import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.joern.pysrc2cpg.Py2CpgTestContext
import io.shiftleft.semanticcpg.language.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class BoolOpCpgTests extends AnyFreeSpec with Matchers {
  lazy val cpg = Py2CpgTestContext.buildCpg("""x or y or z""".stripMargin)

  "test boolOp 'or' call node properties" in {
    val orCall = cpg.call.methodFullName(Operators.logicalOr).head
    orCall.code shouldBe "x or y or z"
    orCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    orCall.lineNumber shouldBe Some(1)
    // TODO orCall.columnNumber shouldBe Some(3)
  }

  "test boolOp 'or' ast children" in {
    cpg.call
      .methodFullName(Operators.logicalOr)
      .astChildren
      .order(1)
      .isIdentifier
      .head
      .code shouldBe "x"
    cpg.call
      .methodFullName(Operators.logicalOr)
      .astChildren
      .order(2)
      .isIdentifier
      .head
      .code shouldBe "y"
    cpg.call
      .methodFullName(Operators.logicalOr)
      .astChildren
      .order(3)
      .isIdentifier
      .head
      .code shouldBe "z"
  }

  "test boolOp 'or' arguments" in {
    cpg.call
      .methodFullName(Operators.logicalOr)
      .argument
      .argumentIndex(1)
      .isIdentifier
      .head
      .code shouldBe "x"
    cpg.call
      .methodFullName(Operators.logicalOr)
      .argument
      .argumentIndex(2)
      .isIdentifier
      .head
      .code shouldBe "y"
    cpg.call
      .methodFullName(Operators.logicalOr)
      .argument
      .argumentIndex(3)
      .isIdentifier
      .head
      .code shouldBe "z"
  }
}
