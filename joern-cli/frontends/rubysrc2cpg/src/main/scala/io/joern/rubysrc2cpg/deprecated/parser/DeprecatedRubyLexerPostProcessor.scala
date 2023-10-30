package io.joern.rubysrc2cpg.deprecated.parser

import io.joern.rubysrc2cpg.deprecated.parser.DeprecatedRubyLexer.*
import org.antlr.v4.runtime.Recognizer.EOF
import org.antlr.v4.runtime.misc.Pair
import org.antlr.v4.runtime.{CommonToken, ListTokenSource, Token, TokenSource}

import scala.::
import scala.jdk.CollectionConverters.*

/** Simplifies the token stream obtained from `DeprecatedRubyLexer`.
  */
object DeprecatedRubyLexerPostProcessor {

  def apply(tokenSource: TokenSource): ListTokenSource = {
    var tokens = tokenSource.toSeq

    tokens = tokens.mergeConsecutive(NON_EXPANDED_LITERAL_CHARACTER, NON_EXPANDED_LITERAL_CHARACTER_SEQUENCE)
    tokens = tokens.mergeConsecutive(EXPANDED_LITERAL_CHARACTER, EXPANDED_LITERAL_CHARACTER_SEQUENCE)
    tokens = tokens.filterNot(_.is(WS))

    new ListTokenSource(tokens.asJava)
  }
}

private implicit class TokenSourceExt(val tokenSource: TokenSource) {

  def toSeq: Seq[Token] = Seq.unfold(tokenSource) { tkSrc =>
    tkSrc.nextToken() match
      case tk if tk.is(EOF) => None
      case tk               => Some((tk, tkSrc))
  }
}

private implicit class SeqExt[A](val elems: Seq[A]) {

  /** An order-preserving `groupBy` implemented on top of `Seq`. Each sub-sequence ("chain") contains 1+ elements. If a
    * chain contains 2+ elements, then all its elements satisfy `p`. Flattening returns the original sequence.
    */
  def chains(p: A => Boolean): Seq[Seq[A]] = elems.foldRight(Nil: Seq[Seq[A]]) { (h, t) =>
    t match
      case chain :: chains if chain.exists(p) && p(h) => (h +: chain) +: chains
      case _                                          => Seq(h) +: t
  }

  /** Collapses, according to a merging operation `m`, all chains that verify `p`.
    */
  def mergeChains(p: A => Boolean, m: Seq[A] => A): Seq[A] = {
    elems.chains(p).flatMap(chain => if (chain.exists(p)) Seq(m(chain)) else chain)
  }

}

private implicit class TokenSeqExt(val tokens: Seq[Token]) {

  def mergeAs(tokenType: Int): Token = {
    val startIndex  = tokens.head.getStartIndex
    val stopIndex   = tokens.last.getStopIndex
    val tokenSource = tokens.head.getTokenSource
    val inputStream = tokens.head.getInputStream
    val channel     = tokens.head.getChannel
    new CommonToken(new Pair(tokenSource, inputStream), tokenType, channel, startIndex, stopIndex)
  }

  def mergeConsecutive(oldTokenType: Int, newTokenType: Int): Seq[Token] = {
    tokens.mergeChains(_.is(oldTokenType), _.mergeAs(newTokenType))
  }
}

private implicit class TokenExt(val token: Token) {

  def is(tokenType: Int): Boolean = token.getType == tokenType

}
