package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class BlockParserTests extends RubyParserFixture with Matchers {
  "Blocks" in {
    test("""a = 42
        |a""".stripMargin)

    test(
      """a
        |b # comment
        |c
        |""".stripMargin,
      """a
        |b
        |c""".stripMargin
    )

    test(
      """a
        |b # comment
        |# another comment
        |c
        |""".stripMargin,
      """a
        |b
        |c""".stripMargin
    )
  }

}
