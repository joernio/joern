package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class InvocationWithoutParenthesesParserTests extends RubyParserFixture with Matchers {
  "method invocation without parenthesis" in {
    test("task.nil?")
    // TODO: Parser error
//    test("do?")

    // TODO: Syntax Error
    test("return!")
  }

  "command with do block" in {
    test("""it 'should print 1' do
        | puts 1
        |end
        |""".stripMargin)

    test("foo&.bar")
    test("foo&.bar 1,2")
  }

  "invocation with association arguments" in {
    // TODO: Syntax error
    test("foo bar:")
  }
}
