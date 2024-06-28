package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class BeginStatementParserTests extends RubyParserFixture with Matchers {
  "BEGIN statement" in {
    test("BEGIN { 1 }")
    test("BEGIN {}")
  }
}
