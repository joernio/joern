package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class ClassDefinitionParserTests extends RubyParserFixture with Matchers {
  "fix tests" ignore {
    // TODO: Syntax error
    test("class X 1 end")
  }

  "class definitions" in {
    test("class << self ; end")
    test("""class << x
        | def show; puts self; end
        |end
        |""".stripMargin)
  }
}
