package io.joern.rubysrc2cpg.deprecated.passes.ast

import io.joern.rubysrc2cpg.testfixtures.{DifferentInNewFrontend, RubyCode2CpgFixture, SameInNewFrontend}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.semanticcpg.language.*

class BoolOpCpgTests extends RubyCode2CpgFixture(useDeprecatedFrontend = true) {
  val cpg = code("""x or y or z""".stripMargin)

  "test boolOp 'or' call node properties" taggedAs SameInNewFrontend in {
    val orCall = cpg.call.head
//    val orCall = cpg.call.methodFullName(Operators.logicalOr).head
    orCall.code shouldBe "x or y or z"
    orCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    orCall.lineNumber shouldBe Some(1)
    // TODO orCall.columnNumber shouldBe Some(3)
  }

  // TODO: Fix this multi logicalOr operation
  "test boolOp 'or' ast children" taggedAs DifferentInNewFrontend ignore {
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

  // TODO: Fix this multi logicalOr operation arguments
  "test boolOp 'or' arguments" taggedAs DifferentInNewFrontend ignore {
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
