package io.joern.rubysrc2cpg.parser

import org.antlr.v4.runtime.{CharStreams, CommonTokenStream, ParserRuleContext}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

abstract class RubyParserAbstractTest extends AnyWordSpec with Matchers {

  def rubyStream(code: String): CommonTokenStream =
    new CommonTokenStream(new RubyLexer(CharStreams.fromString(code)))

  def rubyParser(code: String): RubyParser =
    new RubyParser(rubyStream(code))

  def prettyPrint(withContext: RubyParser => ParserRuleContext, input: String): String =
    AstPrinter.print(withContext(rubyParser(input)))

  def accepts(withContext: RubyParser => ParserRuleContext, input: String): Boolean = {
    val parser = rubyParser(input)
    withContext(parser)
    parser.getNumberOfSyntaxErrors == 0
  }
}
