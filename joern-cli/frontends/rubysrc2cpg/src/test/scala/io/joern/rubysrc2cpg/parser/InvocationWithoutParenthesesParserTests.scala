package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class InvocationWithoutParenthesesParserTests extends RubyParserFixture with Matchers {
  "fix tests" ignore {
    // TODO: Parser error
    test("do?")

    // TODO: Syntax Error
    test("return!")
  }

  "method invocation without parenthesis" in {
    test("task.nil?")
  }

  "command with do block" in {
    test("""it 'should print 1' do
        | puts 1
        |end
        |""".stripMargin)

    test("foo&.bar")
    test("foo&.bar 1,2")
  }
}
