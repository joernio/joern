package io.joern.rubysrc2cpg.parser.antlr

import org.antlr.v4.runtime.{CharStreams, CommonTokenStream, ParserRuleContext}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.util.stream.Collectors

abstract class RubyParserAbstractTest extends AnyWordSpec with Matchers {

  def rubyStream(code: String): CommonTokenStream =
    new CommonTokenStream(new RubyLexer(CharStreams.fromString(code)))

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
