package io.joern.pysrc2cpg.cpg

import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import io.joern.pysrc2cpg.testfixtures.PySrc2CpgFixture

class SetCpgTests extends PySrc2CpgFixture with Matchers {

  "set" should {
    val cpg = code("""{1}""".stripMargin)

    "be represented as `setLiteral`" in {
      val callOption = cpg.call.methodFullName("<operator>.setLiteral").nextOption()
      callOption.isDefined shouldBe true
      callOption.get.code shouldBe "{1}"
    }
  }
}
