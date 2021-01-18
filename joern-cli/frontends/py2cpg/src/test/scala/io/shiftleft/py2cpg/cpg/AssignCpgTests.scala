package io.shiftleft.py2cpg.cpg

import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.py2cpg.Py2CpgTestContext
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class AssignCpgTests extends AnyWordSpec with Matchers {
  val testContext = Py2CpgTestContext.newContext.addSource(
    """x = 2""".stripMargin
  )

  "test single target assign call node properties" in {
    val cpg = testContext.buildCpg

    val assignCall = cpg.call.methodFullName(Operators.assignment).head
    assignCall.code shouldBe "x = 2"
    assignCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    assignCall.lineNumber shouldBe Some(1)
    assignCall.columnNumber shouldBe Some(3)
  }

  "test single target assign call ast children" in {
    val cpg = testContext.buildCpg

    cpg.call.methodFullName(Operators.assignment).astChildren.order(1).isIdentifier.head.code shouldBe "x"
    cpg.call.methodFullName(Operators.assignment).astChildren.order(2).isLiteral.head.code shouldBe "2"
  }

  "test single target assign call arguments" in {
    val cpg = testContext.buildCpg

    cpg.call.methodFullName(Operators.assignment).argument.argumentIndex(1).isIdentifier.head.code shouldBe "x"
    cpg.call.methodFullName(Operators.assignment).argument.argumentIndex(2).isLiteral.head.code shouldBe "2"
  }
}
