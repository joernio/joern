package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class BeginStatementParserTests extends RubyParserFixture with Matchers {
  // TODO: Syntax Errors
  "BEGIN statement" ignore {
    test("BEGIN { 1 }")
    test("BEGIN {}")
  }

  // TODO: Syntax errors
  "END statement" ignore {
    test("END { 1 }")
    test("END {}")
  }
}
