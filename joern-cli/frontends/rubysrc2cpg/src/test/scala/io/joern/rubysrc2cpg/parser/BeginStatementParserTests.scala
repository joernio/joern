package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class BeginStatementParserTests extends RubyParserFixture with Matchers {
  "BEGIN statement" in {
    // TODO: Fix - valid for Ruby 2, but not 3
//    test("BEGIN { 1 }")
//    test("BEGIN {}")
  }
}
