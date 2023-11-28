package io.joern.rubysrc2cpg.deprecated.passes.ast

import io.joern.rubysrc2cpg.testfixtures.{RubyCode2CpgFixture, SameInNewFrontend}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.semanticcpg.language.*

class AttributeCpgTests extends RubyCode2CpgFixture(useDeprecatedFrontend = true) {
  val cpg = code("""x.y""".stripMargin)

  // TODO: Class Modeling testcase
  "test field access call node properties" taggedAs SameInNewFrontend ignore {
    val callNode = cpg.call.methodFullName(Operators.fieldAccess).head
    callNode.code shouldBe "x.y"
    callNode.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    callNode.lineNumber shouldBe Some(1)
  }

  // TODO: Class Modeling testcase
  "test field access call ast children" ignore {
    cpg.call
      .methodFullName(Operators.fieldAccess)
      .astChildren
      .order(1)
      .isIdentifier
      .head
      .code shouldBe "x"
    cpg.call
      .methodFullName(Operators.fieldAccess)
      .astChildren
      .order(2)
      .isFieldIdentifier
      .head
      .code shouldBe "y"
  }

  // TODO: Class Modeling testcase
  "test field access call arguments" ignore {
    cpg.call
      .methodFullName(Operators.fieldAccess)
      .argument
      .argumentIndex(1)
      .isIdentifier
      .head
      .code shouldBe "x"
    cpg.call
      .methodFullName(Operators.fieldAccess)
      .argument
      .argumentIndex(2)
      .isFieldIdentifier
      .head
      .code shouldBe "y"
  }
}
