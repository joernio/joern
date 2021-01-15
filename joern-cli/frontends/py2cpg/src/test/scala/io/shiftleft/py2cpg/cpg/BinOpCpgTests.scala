package io.shiftleft.py2cpg.cpg

import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.py2cpg.Py2CpgTestContext
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BinOpCpgTests extends AnyWordSpec with Matchers {
  val testContext = Py2CpgTestContext.newContext.addSource(
    """1 + 2""".stripMargin
  )

  "test binOp 'add' call node properties" in {
    val cpg = testContext.buildCpg

    val additionCall = cpg.call.methodFullName(Operators.addition).head
    additionCall.code shouldBe "1 + 2"
    additionCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    additionCall.lineNumber shouldBe Some(1)
    // TODO additionCall.columnNumber shouldBe Some(1)
  }

  "test binOp 'add' ast children" in {
    val cpg = testContext.buildCpg

    cpg.call.methodFullName(Operators.addition).astChildren.order(1).isLiteral.head.code shouldBe "1"
    cpg.call.methodFullName(Operators.addition).astChildren.order(2).isLiteral.head.code shouldBe "2"
  }

  "test binOp 'add' arguments" in {
    val cpg = testContext.buildCpg

    cpg.call.methodFullName(Operators.addition).argument.argumentIndex(1).isLiteral.head.code shouldBe "1"
    cpg.call.methodFullName(Operators.addition).argument.argumentIndex(2).isLiteral.head.code shouldBe "2"
  }
}
