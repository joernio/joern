package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class ClassDefinitionParserTests extends RubyParserFixture with Matchers {
  "class definitions" in {
    test("class << self ; end")
    test("class X 1 end")
    test("""class << x
        | def show; puts self; end
        |end
        |""".stripMargin)
  }
}
