package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.JimpleCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class LiteralTests extends JimpleCode2CpgFixture {
  "inner text in literal strings" in {
    val tripQuote = "\"\"\""
    val cpg = code(s"""
         |class Foo {
         | String a = "abc";
         | String b = "\\\"abc";
         | String c = "abc\\\"";
         | String d = ${tripQuote}
         |abc
         |def
         |${tripQuote};
         |}
         |""".stripMargin)

    cpg.literal.strippedCode.l shouldBe List("abc", "\\\"abc", "abc\\\"", "abc\\ndef\\n")
  }
}
