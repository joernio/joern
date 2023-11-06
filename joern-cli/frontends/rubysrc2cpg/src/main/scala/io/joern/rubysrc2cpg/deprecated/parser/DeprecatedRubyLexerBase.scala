package io.joern.rubysrc2cpg.deprecated.parser

import io.joern.rubysrc2cpg.deprecated.parser.DeprecatedRubyLexer.*
import org.antlr.v4.runtime.Recognizer.EOF
import org.antlr.v4.runtime.{CharStream, Lexer, Token}

/** Aggregates auxiliary features to DeprecatedRubyLexer in a single place. */
abstract class DeprecatedRubyLexerBase(input: CharStream)
    extends Lexer(input)
    with RegexLiteralHandling
    with InterpolationHandling
    with QuotedLiteralHandling
    with HereDocHandling {

  /** The previously (non-WS) emitted token (in DEFAULT_CHANNEL.) */
  protected var previousNonWsToken: Option[Token] = None

  /** The previously emitted token (in DEFAULT_CHANNEL.) */
  protected var previousToken: Option[Token] = None

  // Same original behaviour, just updating `previous{NonWs}Token`.
  override def nextToken: Token = {
    val token: Token = super.nextToken
    if (token.getChannel == Token.DEFAULT_CHANNEL && token.getType != WS) {
      previousNonWsToken = Some(token)
    }
    previousToken = Some(token)
    token
  }

  def previousNonWsTokenTypeOrEOF(): Int = {
    previousNonWsToken.map(_.getType).getOrElse(EOF)
  }

  def previousTokenTypeOrEOF(): Int = {
    previousToken.map(_.getType).getOrElse(EOF)
  }

  def isNumericTokenType(tokenType: Int): Boolean = {
    val numericTokenTypes = Set(
      DECIMAL_INTEGER_LITERAL,
      OCTAL_INTEGER_LITERAL,
      HEXADECIMAL_INTEGER_LITERAL,
      FLOAT_LITERAL_WITHOUT_EXPONENT,
      FLOAT_LITERAL_WITH_EXPONENT
    )
    numericTokenTypes.contains(tokenType)
  }
}
