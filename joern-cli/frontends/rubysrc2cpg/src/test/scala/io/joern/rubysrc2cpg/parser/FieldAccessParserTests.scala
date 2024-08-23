package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class FieldAccessParserTests extends RubyParserFixture with Matchers {
  "Normal field access" in {
    test("x.y")
    test("self.x")
    test(
      """a
        |.b
        |.c""".stripMargin,
      "a.b.c"
    )
    test(
      """a
        |.b
        |#.c
        |.d
        |""".stripMargin,
      "a.b.d"
    )
    test(
      """a.
        |b""".stripMargin,
      "a.b"
    )
  }
}
