package io.joern.pysrc2cpg.cpg

import io.joern.x2cpg.frontendspecific.pysrc2cpg.Constants
import io.shiftleft.semanticcpg.language.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.joern.pysrc2cpg.testfixtures.PySrc2CpgFixture

class StrLiteralCpgTests extends PySrc2CpgFixture with Matchers {
  val cpg = code(""""abc"""".stripMargin)

  "test string literal node properties" in {
    val literal = cpg.literal.head
    literal.code shouldBe "\"abc\""
    literal.typeFullName shouldBe Constants.ANY
    literal.dynamicTypeHintFullName should contain only (Constants.builtinStrType)
    literal.lineNumber shouldBe Some(1)
    literal.columnNumber shouldBe Some(1)
  }

  "inner text in string literal" in {
    val cpg2 = code(s"""
        |a = "abc"
        |b = "\\\"abc"
        |c = "abc\\\""
        |d = 'abc'
        |e = '\\'abc'
        |f = 'abc\\''
        |g = \"\"\"
        |abc
        |def
        |\"\"\"
        |h = '''
        |abc
        |def
        |'''
        |""".stripMargin)

    cpg2.literal.strippedCode.l shouldBe List(
      "abc",
      "\\\"abc",
      "abc\\\"",
      "abc",
      "\\'abc",
      "abc\\'",
      """
          |abc
          |def
          |""".stripMargin,
      """
          |abc
          |def
          |""".stripMargin
    )
  }
}
