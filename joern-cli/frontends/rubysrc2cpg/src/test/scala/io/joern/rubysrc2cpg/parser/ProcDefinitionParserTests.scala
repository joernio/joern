package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class ProcDefinitionParserTests extends RubyParserFixture with Matchers {
  "one-line proc definition" in {
    test("-> {}")
    test("-> do ; end")
    test("-> do 1 end")
    test("-> (x) {}")
    test("-> (x) do ; end")
    test("->(x = 1) {}")
    test("-> (foo: 1) do ; end")
    test("->(x, y) {puts x; puts y}")
  }
}
