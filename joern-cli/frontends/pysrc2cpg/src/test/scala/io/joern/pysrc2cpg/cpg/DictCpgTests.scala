package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.testfixtures.Py2CpgTestContext
import io.joern.pysrc2cpg.testfixtures.PySrc2CpgFixture
import io.shiftleft.semanticcpg.language.*

class DictCpgTests extends PySrc2CpgFixture(withOssDataflow = false) {

  "empty dict" should {
    lazy val cpg = Py2CpgTestContext.buildCpg("""{}""".stripMargin)

    "be represented as `dictLiteral`" in {
      val callOption = cpg.call.methodFullName("<operator>.dictLiteral").nextOption()
      callOption.isDefined shouldBe true
      callOption.get.code shouldBe "{}"
    }
  }
  "Truncate large dict" should {
    lazy val cpg = code(s"dict = {${("k" * 1001).map(k => s"$k: v").mkString(", ")}}")

    "more than 1000 key value pairs`" in {
      cpg.call.codeExact("<too-many-key-value-pairs>").size shouldBe 1
    }
  }

}
