package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.testfixtures.Py2CpgTestContext
import io.shiftleft.semanticcpg.language.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class MethodCpgTests extends AnyFreeSpec with Matchers {
  "A method" - {

    lazy val cpg = Py2CpgTestContext.buildCpg(
      """
        |def method():
        |   pass
        |""".stripMargin,
      "a/b.py"
    )

    "test method full name" in {
      val method = cpg.method.name("method").head
      method.fullName shouldBe "a/b.py:<module>.method"
    }
  }

}
