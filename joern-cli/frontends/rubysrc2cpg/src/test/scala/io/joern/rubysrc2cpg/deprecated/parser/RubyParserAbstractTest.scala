package io.joern.rubysrc2cpg.deprecated.parser

import io.joern.rubysrc2cpg.parser.AnltrAstPrinter
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream, ParserRuleContext}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.util.stream.Collectors

// TODO: Should share the same lexer/token stream/parser as the frontend itself.
//  See `io.joern.rubysrc2cpg.astcreation.AntlrParser`
abstract class RubyParserAbstractTest extends AnyWordSpec with Matchers {

  def rubyStream(code: String): CommonTokenStream =
    new CommonTokenStream(DeprecatedRubyLexerPostProcessor(new DeprecatedRubyLexer(CharStreams.fromString(code))))

  def rubyParser(code: String): DeprecatedRubyParser =
    new DeprecatedRubyParser(rubyStream(code))

  def printAst(withContext: DeprecatedRubyParser => ParserRuleContext, input: String): String =
    omitWhitespaceLines(AnltrAstPrinter.print(withContext(rubyParser(input))))

  private def omitWhitespaceLines(text: String): String =
    text.lines().filter(_.strip().nonEmpty).collect(Collectors.joining("\n"))

  def accepts(withContext: DeprecatedRubyParser => ParserRuleContext, input: String): Boolean = {
    val parser = rubyParser(input)
    withContext(parser)
    parser.getNumberOfSyntaxErrors == 0
  }
}
