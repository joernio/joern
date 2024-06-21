package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.Py2CpgTestContext
import io.joern.x2cpg.frontendspecific.pysrc2cpg.Constants
import io.shiftleft.semanticcpg.language.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class StrLiteralCpgTests extends AnyFreeSpec with Matchers {
  lazy val cpg = Py2CpgTestContext.buildCpg(""""abc"""".stripMargin)

  "test string literal node properties" in {
    val literal = cpg.literal.head
    literal.code shouldBe "\"abc\""
    literal.typeFullName shouldBe Constants.ANY
    literal.dynamicTypeHintFullName should contain only (Constants.builtinStrType)
    literal.lineNumber shouldBe Some(1)
    literal.columnNumber shouldBe Some(1)
  }
}
