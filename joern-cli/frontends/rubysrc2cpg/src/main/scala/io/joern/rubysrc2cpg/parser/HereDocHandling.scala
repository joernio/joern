package io.joern.rubysrc2cpg.parser

import better.files.EOF

trait HereDocHandling { this: RubyLexerBase =>

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
      // Count WS characters to ignore
      var idxWs   = 0
      var wsCount = 0

      while (this._input.LA(idxWs + 1).toChar.isWhitespace) {
        wsCount += 1
        idxWs += 1
      }

      // Get the delimiter
      HereDocHandling.getHereDocDelimiter(partialHeredoc) match
        case Some(delimiter) if !delimiter.zipWithIndex.exists { case (c, idx) =>
              this._input.LA(idx + wsCount + 1) != c
            } =>
          // If we get to this point, we know there is an end delimiter ahead in the char stream, make
          // sure it is followed by a white space (or the EOF). If we don't do this, then "FOOS" would also
          // be considered the end for the delimiter "FOO"
          val charAfterDelimiter = this._input.LA(delimiter.length + wsCount + 1)
          charAfterDelimiter == EOF || Character.isWhitespace(charAfterDelimiter)
        case _ => false
    }
}

object HereDocHandling {
  def getHereDocDelimiter(hereDoc: String): Option[String] =
    hereDoc.split("\r?\n|\r").headOption.map(_.replaceAll("^<<[~-]\\s*", ""))
}
