package io.joern.rubysrc2cpg.deprecated.passes.ast

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies, NodeTypes, DispatchTypes, Operators, nodes}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class BinOpCpgTests extends RubyCode2CpgFixture(useDeprecatedFrontend = true) {
  val cpg = code("""1 + 2""".stripMargin)

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
