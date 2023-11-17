package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.Py2CpgTestContext
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DictCpgTests extends AnyWordSpec with Matchers {

  "empty dict" should {
    lazy val cpg = Py2CpgTestContext.buildCpg("""{}""".stripMargin)

    "be represented as `dictLiteral`" in {
      val callOption = cpg.call.methodFullName("<operator>.dictLiteral").nextOption()
      callOption.isDefined shouldBe true
      callOption.get.code shouldBe "{}"
    }
  }
}
