package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.testfixtures.Py2CpgTestContext
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.semanticcpg.language.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SubscriptCpgTests extends AnyFreeSpec with Matchers {
  lazy val cpg = Py2CpgTestContext.buildCpg("""x[1]""".stripMargin)

  "test index access node properties" in {
    val assignCall = cpg.call.methodFullName(Operators.indexAccess).head
    assignCall.code shouldBe "x[1]"
    assignCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    assignCall.lineNumber shouldBe Some(1)
    assignCall.columnNumber shouldBe Some(1)
  }

  "test index access node ast children" in {
    cpg.call
      .methodFullName(Operators.indexAccess)
      .astChildren
      .order(1)
      .isIdentifier
      .head
      .code shouldBe "x"
    cpg.call
      .methodFullName(Operators.indexAccess)
      .astChildren
      .order(2)
      .isLiteral
      .head
      .code shouldBe "1"
  }

  "test index access node arguments" in {
    cpg.call
      .methodFullName(Operators.indexAccess)
      .argument
      .argumentIndex(1)
      .isIdentifier
      .head
      .code shouldBe "x"
    cpg.call
      .methodFullName(Operators.indexAccess)
      .argument
      .argumentIndex(2)
      .isLiteral
      .head
      .code shouldBe "1"
  }
}
