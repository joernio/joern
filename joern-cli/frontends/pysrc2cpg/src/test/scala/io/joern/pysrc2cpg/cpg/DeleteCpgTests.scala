package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.testfixtures.Py2CpgTestContext
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.semanticcpg.language.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class DeleteCpgTests extends AnyFreeSpec with Matchers {
  "delete statement" - {
    lazy val cpg = Py2CpgTestContext.buildCpg("""del x, y""".stripMargin)

    "test call node properties" in {
      val callNode = cpg.call.methodFullNameExact("<operator>.delete").head
      callNode.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      callNode.code shouldBe "del x, y"
      callNode.lineNumber shouldBe Some(1)
    }

    "test delete call node ast children" in {
      cpg.call
        .methodFullName("<operator>.delete")
        .astChildren
        .order(1)
        .isIdentifier
        .head
        .code shouldBe "x"
      cpg.call
        .methodFullName("<operator>.delete")
        .astChildren
        .order(2)
        .isIdentifier
        .head
        .code shouldBe "y"
    }

    "test delete call node arguments" in {
      cpg.call
        .methodFullName("<operator>.delete")
        .argument
        .argumentIndex(1)
        .isIdentifier
        .head
        .code shouldBe "x"
      cpg.call
        .methodFullName("<operator>.delete")
        .argument
        .argumentIndex(2)
        .isIdentifier
        .head
        .code shouldBe "y"
    }
  }

}
