package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class InvocationWithParenthesisParserTests extends RubyParserFixture with Matchers {
  "fixme" ignore {
    test("foo.()") // syntax error
    test("x(&)")   // Syntax error
  }

  "method invocation with parenthesis" in {
    test("defined?(42)")
    test("foo()")
    test("foo([:c => 1, :d])", "foo([:c=> 1,:d])")
    test(
      """foo(
        |)
        |""".stripMargin,
      "foo()"
    )
    test("foo(1)")
    test("foo(region: 1)")
    test("foo(region:region)", "foo(region: region)")
    test("foo(id: /.*/)")
    test("foo(*x, y)", "foo(*x,y)")
    test("foo(:region)")
    test("foo(:region,)", "foo(:region)")
    test("foo(if: true)")
    test("foo(1, 2=>3)", "foo(2=> 3,1)")
    test("foo(1, 2=>3,)", "foo(2=> 3,1)")
    test("foo(1=> 2,)", "foo(1=> 2)")
    test("foo(1, kw: 2, **3)", "foo(**3,kw: 2,1)")
    test("foo(b, **1)", "foo(**1,b)")
    test("""foo(b: if :c
        |1
        |else
        |2
        |end)""".stripMargin)
    test("foo&.bar()")
    test("foo&.bar(1, 2)", "foo&.bar(1,2)")
    test(
      """foo
        |.bar
        |""".stripMargin,
      "foo.bar"
    )
    test(
      """foo.
        |bar
        |""".stripMargin,
      "foo.bar"
    )

    test("f(1, kw:2, **3)", "f(**3,kw: 2,1)")
  }

  "Method with comments" in {
    test(
      """# blah 1
        |# blah 2
        |def blah
        |end""".stripMargin,
      """def blah
      |end""".stripMargin
    )
  }
}
