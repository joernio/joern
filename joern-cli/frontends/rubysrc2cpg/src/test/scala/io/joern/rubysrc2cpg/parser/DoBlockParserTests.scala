package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class DoBlockParserTests extends RubyParserFixture with Matchers {
  "test" in {}

  "Some block" in {
    test(
      "def foo &block;end",
      """def foo(&block)
        |end""".stripMargin
    )
    test("""arr.each { |item| }""", """arr.each {|item|}""")
    test(
      """hash.each do |key, value|
           |end
           |""".stripMargin,
      """hash.each do |key,value|
        |end""".stripMargin
    )
    test(
      s"x = proc { \"Hello #{myValue}\" }",
      """x = proc {
      |"Hello #{myValue}"
      |}""".stripMargin
    )
    test(
      "Array.new(x) { |i| i += 1 }",
      """Array.new(x) {|i|
        |i += 1
        |}""".stripMargin
    )
    test(
      "test_name 'Foo' do;end",
      """test_name 'Foo' do
        |end""".stripMargin
    )
  }
}
