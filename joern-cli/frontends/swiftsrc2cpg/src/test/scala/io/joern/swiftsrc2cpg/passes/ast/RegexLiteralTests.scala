package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.semanticcpg.language.*

class RegexLiteralTests extends SwiftSrc2CpgSuite {

  "RegexLiteralTests" should {

    "testRegexLiteral" in {
      val cpg = code("##/abc/#def/##")
      cpg.unknown.code.l shouldBe List("##/abc/#def/##")
    }

  }
}
