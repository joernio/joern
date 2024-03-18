package io.joern.jssrc2cpg.io

import io.joern.jssrc2cpg.testfixtures.AstJsSrc2CpgSuite
import io.shiftleft.semanticcpg.language.*

class MinifiedFileDetectionTests extends AstJsSrc2CpgSuite {

  "Detecting minified files" should {
    "skip minified files by name correctly" in {
      val cpg = code("", "a.min.js")
        .moreCode("", "a.min.23472420.js")
        .moreCode("", "b-min.js")
        .moreCode("", "b-min.23472420.js")
        .moreCode("", "c.bundle.js")
      cpg.file.name.l shouldBe empty
    }

    "skip minified files by content correctly" in {
      val cpg = code(s"console.log('${"x" * 10000}');")
      cpg.file.name.l shouldBe empty
    }

  }

}
