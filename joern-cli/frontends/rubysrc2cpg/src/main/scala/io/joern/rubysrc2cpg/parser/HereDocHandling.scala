package io.joern.rubysrc2cpg.parser

import better.files.EOF

trait HereDocHandling { this: RubyLexerBase =>

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

}
