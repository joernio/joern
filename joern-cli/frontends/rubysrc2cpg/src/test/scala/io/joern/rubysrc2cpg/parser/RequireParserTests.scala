package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class RequireParserTests extends RubyParserFixture with Matchers {
  "require" in {
    test("require sendgrid-ruby")
    test("require_all './dir'")
    test("require_relative 'util/help/dir/'")
  }
}
