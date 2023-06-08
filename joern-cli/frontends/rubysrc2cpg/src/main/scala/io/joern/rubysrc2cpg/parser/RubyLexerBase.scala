package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.parser.RubyLexer._
import org.antlr.v4.runtime.{CharStream, Lexer, Token}

/** Aggregates auxiliary features to RubyLexer in a single place. */
abstract class RubyLexerBase(input: CharStream)
    extends Lexer(input)
    with RubyLexerRegexHandling
    with RubyLexerStringInterpolationHandling {

  /** The previously (non-WS) emitted token (in DEFAULT_CHANNEL.) */
  protected var previousNonWsToken: Option[Token] = None

  // Same original behaviour, just updating `previousNonWsToken`.
  override def nextToken: Token = {
    val token: Token = super.nextToken
    if (token.getChannel == Token.DEFAULT_CHANNEL && token.getType != WS) {
      previousNonWsToken = Some(token)
    }
    token
  }

}
