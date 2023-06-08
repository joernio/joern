package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.parser.RubyLexer._
import org.antlr.v4.runtime.{CharStream, Lexer, Token}

/** Aggregates auxiliary features to RubyLexer in a single place. */
abstract class RubyLexerBase(input: CharStream) extends Lexer(input) {

  /* When encountering '/', we need to decide whether this is a binary operator (e.g. `x / y`) or
   * a regular expression delimiter (e.g. `/(eu|us)/`) occurrence. Our approach is to look at the
   * previously emitted token and decide accordingly.
   */
  private val regexTogglingTokens: Set[Integer] = Set(
    // When '/' is the first token in the stream.
    null,
    // When '/' occurs after an opening parenthesis, brace or bracket.
    LPAREN,
    LCURLY,
    LBRACK,
    // When '/' occurs after a NL.
    NL,
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

  /** The previously (non-WS) emitted token (in DEFAULT_CHANNEL.) */
  private var previousNonWsToken: Token = null

  // Same original behaviour, just updating `previousNonWsToken`.
  override def nextToken: Token = {
    val token: Token = super.nextToken
    if (token.getChannel == Token.DEFAULT_CHANNEL && token.getType != WS) previousNonWsToken = token
    token
  }

  /** To be invoked when encountering `/`, deciding if it should emit a `REGULAR_EXPRESSION_START` token.
    */
  protected def isStartOfRegex: Boolean = {
    val previousNonWsTokenType: Integer =
      if (previousNonWsToken == null) null
      else previousNonWsToken.getType
    regexTogglingTokens.contains(previousNonWsTokenType)
  }

  /** To be invoked when in `DEFAULT_MODE`, to check if we are in the context of a string interpolation. */
  protected def isInStringInterpolationMode: Boolean =
    _modeStack.size > 1 && _modeStack.peek == DOUBLE_QUOTED_STRING_MODE

  /** To be invoked when in `DEFAULT_MODE`, to check if we are in the context of a regular expression interpolation. */
  protected def isInRegularExpressionInterpolationMode: Boolean =
    _modeStack.size > 1 && _modeStack.peek == REGULAR_EXPRESSION_MODE

}
