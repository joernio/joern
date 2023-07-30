package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.parser.RubyLexer._

trait RubyLexerStringInterpolationHandling { this: RubyLexerBase =>

  /** To be invoked when in `DEFAULT_MODE`, to check if we are in the context of a string interpolation. */
  protected def isInStringInterpolationMode: Boolean =
    _modeStack.size > 1 && _modeStack.peek == DOUBLE_QUOTED_STRING_MODE

}
