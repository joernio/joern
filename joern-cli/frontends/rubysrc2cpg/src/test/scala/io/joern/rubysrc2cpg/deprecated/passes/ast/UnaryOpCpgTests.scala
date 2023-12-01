package io.joern.rubysrc2cpg.deprecated.passes.ast

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.semanticcpg.language.*

class UnaryOpCpgTests extends RubyCode2CpgFixture(useDeprecatedFrontend = true) {
  "#unaryOp not" should {
    val cpg = code("""!true""".stripMargin)
    "test unaryOp 'not' call node properties" in {
      val plusCall = cpg.call.methodFullName(Operators.not).head
      plusCall.code shouldBe "!true"
      plusCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      plusCall.lineNumber shouldBe Some(1)
    }

    "test unaryOp 'not' arguments" in {
      cpg.call
        .methodFullName(Operators.not)
        .argument
        .argumentIndex(1)
        .isLiteral
        .head
        .code shouldBe "true"
    }
  }

  "#unaryOp invert" should {
    val cpg = code("""~2""".stripMargin)
    "test unaryOp 'invert' call node properties" in {
      val plusCall = cpg.call.methodFullName(Operators.not).head
      plusCall.code shouldBe "~2"
      plusCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      plusCall.lineNumber shouldBe Some(1)
    }

    "test unaryOp 'invert' arguments" in {
      cpg.call
        .methodFullName(Operators.not)
        .argument
        .argumentIndex(1)
        .isLiteral
        .head
        .code shouldBe "2"
    }
  }
}
