package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.testfixtures.Py2CpgTestContext
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class AssertCpgTests extends AnyFreeSpec with Matchers {

  lazy val cpg = Py2CpgTestContext.buildCpg("""assert x, y """.stripMargin)

  "test assert operator call node properties" in {
    val assignCall = cpg.call.methodFullName("<operator>.assert").head
    assignCall.code shouldBe "assert x, y"
    assignCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    assignCall.lineNumber shouldBe Some(1)
    assignCall.columnNumber shouldBe Some(1)
  }

  "test assert operator node ast children" in {
    cpg.call
      .methodFullName("<operator>.assert")
      .astChildren
      .order(1)
      .isIdentifier
      .head
      .code shouldBe "x"
    cpg.call
      .methodFullName("<operator>.assert")
      .astChildren
      .order(2)
      .isIdentifier
      .head
      .code shouldBe "y"
  }

  "test assert operator node arguments" in {
    cpg.call
      .methodFullName("<operator>.assert")
      .argument
      .argumentIndex(1)
      .isIdentifier
      .head
      .code shouldBe "x"
    cpg.call
      .methodFullName("<operator>.assert")
      .argument
      .argumentIndex(2)
      .isIdentifier
      .head
      .code shouldBe "y"
  }
}
