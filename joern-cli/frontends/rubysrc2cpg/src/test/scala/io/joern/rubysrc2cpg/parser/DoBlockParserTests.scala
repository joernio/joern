package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class DoBlockParserTests extends RubyParserFixture with Matchers {
  "Some block" in {
    test("def foo &block;end")
    test("""arr.each { |item| }""")
    test("""hash.each do |key, value|
        |end
        |""".stripMargin)
    test(s"x = proc { \"Hello #{myValue}\" }")
    test("Array.new(x) { |i| i += 1 }")
    test("test_name 'Foo' do;end")
  }
}
