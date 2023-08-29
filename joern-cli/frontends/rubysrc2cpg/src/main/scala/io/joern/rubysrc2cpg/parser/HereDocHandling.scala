package io.joern.rubysrc2cpg.parser

import better.files.EOF

import scala.collection.mutable

trait HereDocHandling { this: RubyLexerBase =>

  private def hereDocStack = mutable.Stack[String]()

  /** @see
    *   <a
    *   href="https://stackoverflow.com/questions/66406688/antlr-lexing-bash-files-especially-heredoc/66407350#66407350">Stack
    *   Overflow</a>
    */
  def heredocEndAhead(partialHeredoc: String): Boolean =
    if (this.getCharPositionInLine != 0) {
      // If the lexer is not at the start of a line, no end-delimiter can be possible
      false
    } else {
      // Get the delimiter
      val firstLine = partialHeredoc.split("\r?\n|\r")(0)
      val delimiter = firstLine.replaceAll("^<<-\\s*", "")

      if (delimiter.zipWithIndex.exists { case (c, idx) => this._input.LA(idx + 1) != c }) {
        false
      } else {
        // If we get to this point, we know there is an end delimiter ahead in the char stream, make
        // sure it is followed by a white space (or the EOF). If we don't do this, then "FOOS" would also
        // be considered the end for the delimiter "FOO"
        val charAfterDelimiter = this._input.LA(delimiter.length + 1)
        charAfterDelimiter == EOF || Character.isWhitespace(charAfterDelimiter)
      }
    }

  def heredocEndAhead(): Boolean = hereDocStack.headOption match
    case Some(_) => heredocEndAhead(hereDocStack.pop())
    case None    => false

  def pushHereDocStack(hereDocIdentifier: String): Unit = {
    val delimiter = hereDocIdentifier.replaceAll("^<<-\\s*", "")
    hereDocStack.push(delimiter)
  }

}
