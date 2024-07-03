package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class ModuleParserTests extends RubyParserFixture with Matchers {
  "Module Definition" in {
    test("module Bar; end")
  }
}
