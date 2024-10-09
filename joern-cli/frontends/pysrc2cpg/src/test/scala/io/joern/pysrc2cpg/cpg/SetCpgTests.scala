package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.testfixtures.Py2CpgTestContext
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SetCpgTests extends AnyWordSpec with Matchers {

  "set" should {
    lazy val cpg = Py2CpgTestContext.buildCpg("""{1}""".stripMargin)

    "be represented as `setLiteral`" in {
      val callOption = cpg.call.methodFullName("<operator>.setLiteral").nextOption()
      callOption.isDefined shouldBe true
      callOption.get.code shouldBe "{1}"
    }
  }
}
