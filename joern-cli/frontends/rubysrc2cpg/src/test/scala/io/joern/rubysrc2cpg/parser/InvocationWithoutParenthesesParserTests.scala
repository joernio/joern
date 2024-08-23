package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class InvocationWithoutParenthesesParserTests extends RubyParserFixture with Matchers {
  "method invocation without parenthesis" in {
    test("task.nil?")
    test("foo?")
    test("foo!")
    test("foo a.b 1")
  }

  "command with do block" in {
    test(
      """it 'should print 1' do
        | puts 1
        |end
        |""".stripMargin,
      """it 'should print 1' do
        |puts 1
        |end""".stripMargin
    )

    test("foo&.bar")
    test("foo&.bar 1, 2")
  }

  "method invocation without parenthesis with reserved keywords" in {
    test("batch.retry!")
    test("batch.retry")
  }
}
