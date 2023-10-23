package io.joern.rubysrc2cpg.deprecated.parser

import io.joern.rubysrc2cpg.deprecated.parser.{AstPrinter, RubyLexerPostProcessor}
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream, ParserRuleContext}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.util.stream.Collectors

// TODO: Should share the same lexer/token stream/parser as the frontend itself.
//  See `io.joern.rubysrc2cpg.astcreation.AntlrParser`
abstract class RubyParserAbstractTest extends AnyWordSpec with Matchers {

  def rubyStream(code: String): CommonTokenStream =
    new CommonTokenStream(RubyLexerPostProcessor(new RubyLexer(CharStreams.fromString(code))))

  def rubyParser(code: String): RubyParser =
    new RubyParser(rubyStream(code))

  def printAst(withContext: RubyParser => ParserRuleContext, input: String): String =
    omitWhitespaceLines(AstPrinter.print(withContext(rubyParser(input))))

  private def omitWhitespaceLines(text: String): String =
    text.lines().filter(_.strip().nonEmpty).collect(Collectors.joining("\n"))

  def accepts(withContext: RubyParser => ParserRuleContext, input: String): Boolean = {
    val parser = rubyParser(input)
    withContext(parser)
    parser.getNumberOfSyntaxErrors == 0
  }
}
