package io.joern.pysrc2cpg.cpg

import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language.*
import org.scalatest.Inside.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.joern.pysrc2cpg.testfixtures.PySrc2CpgFixture

class ListCpgTests extends PySrc2CpgFixture with Matchers {
  val cpg = code("""[1,2,3]""".stripMargin)

  "test list expression node properties" in {
    inside(cpg.call.methodFullName("<operator>.listLiteral").l) { case call :: Nil =>
      call.code shouldBe "[1, 2, 3]"
      call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      call.lineNumber shouldBe Some(1)
      call.columnNumber shouldBe Some(1)
    }
  }

  "test list expression arguments" in {
    inside(cpg.call.methodFullName("<operator>.listLiteral").l) { case call :: Nil =>
      val arg1 = call.argument(1)
      arg1.code shouldBe "1"
      arg1.astParent shouldBe call

      val arg2 = call.argument(2)
      arg2.code shouldBe "2"
      arg2.astParent shouldBe call

      val arg3 = call.argument(3)
      arg3.code shouldBe "3"
      arg3.astParent shouldBe call
    }
  }

}
