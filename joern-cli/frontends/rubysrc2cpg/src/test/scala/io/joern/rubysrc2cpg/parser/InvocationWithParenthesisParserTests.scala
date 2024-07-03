package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class InvocationWithParenthesisParserTests extends RubyParserFixture with Matchers {
  "method invocation with parenthesis" in {
    test("foo()")
    test("""foo(
        |)
        |""".stripMargin)
    test("foo(1)")
    test("foo(region: 1)")
    test("foo(region:region)")
    test("foo(id: /.*/)")
    test("foo(*x, y)")
    test("foo(:region)")
    test("foo(:region,)")
    test("foo(if: true)")
    test("foo&.bar()")
    test("foo&.bar(1, 2)")
    test("""foo
        |.bar
        |""".stripMargin)
    test("""foo.
        |bar
        |""".stripMargin)
  }
}
