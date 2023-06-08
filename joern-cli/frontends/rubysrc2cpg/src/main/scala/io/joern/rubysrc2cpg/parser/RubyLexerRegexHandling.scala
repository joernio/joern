package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.parser.RubyLexer._

trait RubyLexerRegexHandling { this: RubyLexerBase =>

  /* When encountering '/', we need to decide whether this is a binary operator (e.g. `x / y`) or
   * a regular expression delimiter (e.g. `/(eu|us)/`) occurrence. Our approach is to look at the
   * previously emitted token and decide accordingly.
   */
  private val regexTogglingTokens: Set[Any] = Set(
    // When '/' is the first token in the stream.
    null,
    // When '/' occurs after an opening parenthesis, brace or bracket.
    LPAREN,
    LCURLY,
    LBRACK,
    // When '/' occurs after a NL.
    NL,
    // When '/' occurs after a ','.
    COMMA,
    // When '/' occurs after an operator.
    EMARK,
    EMARKEQ,
    EMARKTILDE,
    AMP,
    AMP2,
    AMPDOT,
    BAR,
    BAR2,
    EQ,
    EQ2,
    EQ3,
    CARET,
    LTEQGT,
    EQTILDE,
    GT,
    GTEQ,
    LT,
    LTEQ,
    LT2,
    GT2,
    PLUS,
    MINUS,
    STAR,
    STAR2,
    SLASH,
    PERCENT,
    TILDE,
    PLUSAT,
    MINUSAT,
    ASSIGNMENT_OPERATOR
  )

  /** To be invoked when encountering `/`, deciding if it should emit a `REGULAR_EXPRESSION_START` token. */
  protected def isStartOfRegex: Boolean =
    regexTogglingTokens.contains(previousNonWsToken.map(_.getType).orNull)

  /** To be invoked when in `DEFAULT_MODE`, to check if we are in the context of a regular expression interpolation. */
  protected def isInRegularExpressionInterpolationMode: Boolean =
    _modeStack.size > 1 && _modeStack.peek == REGULAR_EXPRESSION_MODE

}
