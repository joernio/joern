package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.parser.{RubyLexer, RubyParser}
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}

import scala.util.Try

/** A consumable wrapper for the RubyParser class used to parse the given file and clear resources held onto by ANTLR.
  * @param filename
  *   the file path to the file to be parsed.
  */
class AntlrParser(filename: String) extends AutoCloseable {

  private val charStream  = CharStreams.fromFileName(filename)
  private val lexer       = new RubyLexer(charStream)
  private val tokenStream = new CommonTokenStream(lexer)
  private val parser      = new RubyParser(tokenStream)

  def parse(): Try[RubyParser.ProgramContext] = Try(parser.program())

  override def close(): Unit =
    parser.getInterpreter.clearDFA()

}
