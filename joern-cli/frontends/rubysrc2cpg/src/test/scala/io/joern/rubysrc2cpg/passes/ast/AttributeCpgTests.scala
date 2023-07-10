package io.joern.rubysrc2cpg.passes.ast

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies, NodeTypes, DispatchTypes, Operators, nodes}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class AttributeCpgTests extends RubyCode2CpgFixture {
  val cpg = code("""x.y""".stripMargin)

  // TODO: Class Modeling testcase
  "test field access call node properties" ignore {
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
