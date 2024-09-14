package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class DoBlockParserTests extends RubyParserFixture with Matchers {
  "fixme" ignore {
    test(
      "break foo arg do |bar| end"
    ) // syntax error - possibly false syntax error due to just having a code sample starting with break which our parser doesn't allow
    test("yield foo arg do |bar| end") // syntax error
    test("a.b do | ; c | end")         // syntax error
  }

  "Some block" in {
    test(
      "f { |a, (b, *, c)| }",
      """f {
        |{|a,(b, c, *)|}
        |}""".stripMargin
    )

    test(
      "a { |b, c=1, *d, e, &f| }",
      """a {
        |{|b,c=1,*d,&f,e|}
        |}""".stripMargin
    )

    test(
      "f { |a, (b, c), d| }",
      """f {
        |{|a,(b, c),d|}
        |}""".stripMargin
    )

    test(
      "a { |b, *c, d| }",
      """a {
        |{|b,*c,d|}
        |}""".stripMargin
    )

    test(
      "f { |(*a)| }",
      """f {
        |{|(*a)|}
        |}""".stripMargin
    )

    // Order on params is out because we sort by line/col in RubyNode creator but we don't have access to that
    // in the AstPrinter
    test(
      "f { |(*, a)| }",
      """f {
        |{|(a, *)|}
        |}""".stripMargin
    )

    test(
      "f { |(a, *b, c)| }",
      """f {
        |{|(a, c, *b)|}
        |}""".stripMargin
    )

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

    test(
      "f { |(*, a)| }",
      """f {
        |{|(a, *)|}
        |}""".stripMargin
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

    test(
      "a { |b, *c, d| }",
      """a {
        |{|b,*c,d|}
        |}""".stripMargin
    )
  }
}
