package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.parser.RubyLexer._
import org.antlr.v4.runtime.Recognizer.EOF

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
    // When '/' occurs after a ':'.
    COLON,
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
    regexTogglingTokens.contains(previousNonWsToken.map(_.getType).orNull) || isInCommandArgumentPosition

  /** Decides if the current `/` is being used as an argument to a command, based on the observation that such literals
    * may not start with a WS. E.g. `puts /x/` is valid, but `puts / x/` is not.
    */
  private def isInCommandArgumentPosition: Boolean = {
    val previousNonWsIsIdentifier = previousNonWsToken.map(_.getType).getOrElse(EOF) == LOCAL_VARIABLE_IDENTIFIER
    val previousIsWs              = previousToken.map(_.getType).getOrElse(EOF) == WS
    val nextIsWs                  = _input.LA(1) == ' '
    previousNonWsIsIdentifier && previousIsWs && !nextIsWs
  }

  /** To be invoked when in `DEFAULT_MODE`, to check if we are in the context of a regular expression interpolation. */
  protected def isInRegularExpressionInterpolationMode: Boolean =
    _modeStack.size > 1 && _modeStack.peek == REGULAR_EXPRESSION_MODE

}
