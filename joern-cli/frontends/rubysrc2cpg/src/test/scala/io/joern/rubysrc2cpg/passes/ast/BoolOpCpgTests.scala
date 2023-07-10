package io.joern.rubysrc2cpg.passes.ast

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies, NodeTypes, DispatchTypes, Operators, nodes}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class BoolOpCpgTests extends RubyCode2CpgFixture {
  val cpg = code("""x or y or z""".stripMargin)

  "test boolOp 'or' call node properties" in {
    val orCall = cpg.call.head
//    val orCall = cpg.call.methodFullName(Operators.logicalOr).head
    orCall.code shouldBe "x or y or z"
    orCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    orCall.lineNumber shouldBe Some(1)
    // TODO orCall.columnNumber shouldBe Some(3)
  }

  // TODO: Fix this multi logicalOr operation
  "test boolOp 'or' ast children" ignore {
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
  "test boolOp 'or' arguments" ignore {
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
