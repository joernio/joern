package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class RangeParserTests extends RubyParserFixture with Matchers {
  "Range Operator" in {
    test("0..", "0..Float::INFINITY")
    test("1..2")
    test(
      """0..
        |4
        |a..
        |b
        |c
        |""".stripMargin,
      """0..4
        |a..b
        |c""".stripMargin
    )
    test(
      """0..
        |;
        |2..
        |""".stripMargin,
      """0..Float::INFINITY
        |2..Float::INFINITY""".stripMargin
    )
    test("..2", "-Float::INFINITY..2")
    test("...3", "-Float::INFINITY...3")
  }
}
