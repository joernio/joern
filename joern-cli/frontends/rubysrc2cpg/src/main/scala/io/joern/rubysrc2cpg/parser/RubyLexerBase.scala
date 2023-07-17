package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.parser.RubyLexer._
import org.antlr.v4.runtime.{CharStream, Lexer, Token}

/** Aggregates auxiliary features to RubyLexer in a single place. */
abstract class RubyLexerBase(input: CharStream)
    extends Lexer(input)
    with RubyLexerRegexHandling
    with RubyLexerStringInterpolationHandling
    with RubyLexerQuotedNonExpandedStringHandling
    with RubyLexerQuotedNonExpandedStringArrayHandling {

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

}
