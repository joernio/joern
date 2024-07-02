package io.joern.pysrc2cpg.cpg

import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.joern.pysrc2cpg.Py2CpgTestContext
import io.shiftleft.semanticcpg.language.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class StarredCpgTests extends AnyFreeSpec with Matchers {
  "starred unpack" - {
    lazy val cpg = Py2CpgTestContext.buildCpg("""func(*x)""".stripMargin)

    "test starred unpack node properties" in {
      val starredUnrollCall = cpg.call.methodFullName("<operator>.starredUnpack").head
      starredUnrollCall.code shouldBe "*x"
      starredUnrollCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      starredUnrollCall.lineNumber shouldBe Some(1)
      starredUnrollCall.columnNumber shouldBe Some(6)
    }

    "test starred unpack ast children" in {
      cpg.call
        .methodFullName("<operator>.starredUnpack")
        .astChildren
        .order(1)
        .isIdentifier
        .head
        .code shouldBe "x"
    }

    "test starred unpack node arguments" in {
      cpg.call
        .methodFullName("<operator>.starredUnpack")
        .argument
        .argumentIndex(1)
        .isIdentifier
        .head
        .code shouldBe "x"
    }
  }
}
