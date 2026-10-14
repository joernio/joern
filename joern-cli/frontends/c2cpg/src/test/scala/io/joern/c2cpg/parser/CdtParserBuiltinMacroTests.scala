package io.joern.c2cpg.parser

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CdtParserBuiltinMacroTests extends AnyWordSpec with Matchers {

  "CdtParser.expandBuiltinMacros" should {

    "expand all four of CDT's built-in macros" in {
      CdtParser
        .expandBuiltinMacros() shouldBe Set("__STDC_VERSION__", "__STDC_HOSTED__", "__CDT_PARSER__", "__cplusplus")
    }
  }

}
