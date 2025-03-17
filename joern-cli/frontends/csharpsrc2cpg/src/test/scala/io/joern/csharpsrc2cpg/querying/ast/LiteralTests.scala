package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class LiteralTests extends CSharpCode2CpgFixture {
  "inner text in string literals" in {
    val cpg = code(basicBoilerplate("""
        |var a = "abc";
        |var b = "\"abc";
        |var c = "abc\"";
        |var d = "\"abc\"";
        |var e = "a\"b\"c";
        |""".stripMargin))

    cpg.literal.innerText.l shouldBe List(
      Some("abc"),
      Some("\\\"abc"),
      Some("abc\\\""),
      Some("\\\"abc\\\""),
      Some("a\\\"b\\\"c")
    )
  }
}
