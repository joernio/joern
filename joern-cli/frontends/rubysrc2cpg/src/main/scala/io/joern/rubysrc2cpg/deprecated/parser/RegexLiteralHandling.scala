package io.joern.rubysrc2cpg.deprecated.parser

import io.joern.rubysrc2cpg.deprecated.parser.DeprecatedRubyLexer.*
import org.antlr.v4.runtime.Recognizer.EOF

trait RegexLiteralHandling { this: DeprecatedRubyLexerBase =>

  /* When encountering '/', we need to decide whether this is a binary operator (e.g. `x / y`) or
   * a regular expression delimiter (e.g. `/(eu|us)/`) occurrence. Our approach is to look at the
   * previously emitted token and decide accordingly.
   */
  private val regexTogglingTokens: Set[Int] = Set(
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
    // When '/' occurs after 'when'.
    WHEN,
    // When '/' occurs after 'unless'.
    UNLESS,
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
  protected def isStartOfRegexLiteral: Boolean = {
    val isFirstTokenInTheStream = previousNonWsToken.isEmpty
    val isRegexTogglingToken    = regexTogglingTokens.contains(previousNonWsTokenTypeOrEOF())

    isFirstTokenInTheStream || isRegexTogglingToken || isInCommandArgumentPosition
  }

  /** Decides if the current `/` is being used as an argument to a command, based on the observation that such literals
    * may not start with a WS. E.g. `puts /x/` is valid, but `puts / x/` is not.
    */
  private def isInCommandArgumentPosition: Boolean = {
    val previousNonWsIsIdentifier = previousNonWsTokenTypeOrEOF() == LOCAL_VARIABLE_IDENTIFIER
    val previousIsWs              = previousTokenTypeOrEOF() == WS
    val nextCharIsWs              = _input.LA(1) == ' '
    previousNonWsIsIdentifier && previousIsWs && !nextCharIsWs
  }

}
