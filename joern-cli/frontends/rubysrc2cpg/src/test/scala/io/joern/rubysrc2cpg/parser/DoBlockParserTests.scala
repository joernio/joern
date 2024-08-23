package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class DoBlockParserTests extends RubyParserFixture with Matchers {
  "fixme" ignore {
    test("f { |(*a)| }")              // syntax error
    test("f { |a, (b, c), d| }")      // syntax error
    test("a { |b, c=1, *d, e, &f| }") // syntax error
    test("a { |b, *c, d| }")          // syntax error
    test(
      "break foo arg do |bar| end"
    ) // syntax error - possibly false syntax error due to just having a code sample starting with break which our parser doesn't allow
    test("f { |(*, a)| }")             // syntax error
    test("f { |(a, *b, c)| }")         // syntax error
    test("yield foo arg do |bar| end") // syntax error
    test("f { |a, (b, *, c)| }")       // syntax error
    test("a.b do | ; c | end")         // syntax error
  }

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
    test(
      "f{ |a, b| }",
      """f {
        |{|a,b|}
        |}""".stripMargin
    )
    test(
      """f do |x, y|
        |x + y
        |end
        |""".stripMargin,
      """f do |x,y|
        |x + y
        |end""".stripMargin
    )

    test("""f(a) do |x,y|
        |x + y
        |end""".stripMargin)

    test(
      """a.b do | | end""".stripMargin,
      """a.b do
        |end""".stripMargin
    )
  }

  "Block arguments" in {
    test(
      "f { |a, b| }",
      """f {
        |{|a,b|}
        |}""".stripMargin
    )

    test(
      "a { |b, c=1, d, &e| }",
      """a {
        |{|b,c=1,d,&e|}
        |}""".stripMargin
    )

    test(
      "a { |b, c=1, *d| }",
      """a {
        |{|b,c=1,*d|}
        |}""".stripMargin
    )

    test(
      "f { |a, b = 42| [a, b] }",
      """f {
        |{|a,b=42|
        |[a,b]
        |}
        |}""".stripMargin
    )

    test(
      "a { | b=1, c=2 | }",
      """a {
        |{|b=1,c=2|}
        |}""".stripMargin
    )

    test(
      "a { |**b | }",
      """a {
        |{|**b|}
        |}""".stripMargin
    )

    test(
      "bl { |kw: :val| kw }",
      """bl {
        |{|kw::val|
        |kw
        |}
        |}""".stripMargin
    )
  }
}
