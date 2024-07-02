package io.joern.pysrc2cpg.cpg

import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.joern.pysrc2cpg.Py2CpgTestContext
import io.shiftleft.semanticcpg.language.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class AttributeCpgTests extends AnyFreeSpec with Matchers {
  lazy val cpg = Py2CpgTestContext.buildCpg("""x.y""".stripMargin)

  "test field access call node properties" in {
    val callNode = cpg.call.methodFullName(Operators.fieldAccess).head
    callNode.code shouldBe "x.y"
    callNode.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    callNode.lineNumber shouldBe Some(1)
  }

  "test field access call ast children" in {
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

  "test field access call arguments" in {
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
