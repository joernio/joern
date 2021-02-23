package io.shiftleft.py2cpg.cpg

import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.py2cpg.Py2CpgTestContext
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CompareCpgTests extends AnyFreeSpec with Matchers {
  "single operation comparison" - {
    lazy val cpg = Py2CpgTestContext.buildCpg(
      """x < y""".stripMargin
    )

    "test compare node" in {
      val callNode = cpg.call.code("x < y").head
      callNode.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      callNode.name shouldBe Operators.lessThan
      callNode.methodFullName shouldBe Operators.lessThan
      callNode.lineNumber shouldBe Some(1)
    }

    "test compare node ast children" in {
      cpg.call.code("x < y").astChildren.order(1).isIdentifier.head.code shouldBe "x"
      cpg.call.code("x < y").astChildren.order(2).isIdentifier.head.code shouldBe "y"
    }

    "test compare node arguments" in {
      cpg.call.code("x < y").argument.argumentIndex(1).isIdentifier.head.code shouldBe "x"
      cpg.call.code("x < y").argument.argumentIndex(2).isIdentifier.head.code shouldBe "y"
    }
  }

  "multi operation comparison" - {
    lazy val cpg = Py2CpgTestContext.buildCpg(
      """x < y < z""".stripMargin
    )

    "test compare node" in {
      // TODO do more than smoke test.
      cpg
    }

  }

}
