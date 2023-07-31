package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.parser.{RubyLexer, RubyParser}
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream, ParserRuleContext, Token}

class AntlrParser {

  def parse(filename: String): RubyParser.ProgramContext = {
    val charStream  = CharStreams.fromFileName(filename)
    val lexer       = new RubyLexer(charStream)
    val tokenStream = new CommonTokenStream(lexer)
    val parser      = new RubyParser(tokenStream)
    parser.program()
  }

}
