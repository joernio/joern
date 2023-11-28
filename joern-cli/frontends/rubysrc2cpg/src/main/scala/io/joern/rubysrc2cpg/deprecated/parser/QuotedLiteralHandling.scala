package io.joern.rubysrc2cpg.deprecated.parser

import scala.collection.mutable

trait QuotedLiteralHandling { this: DeprecatedRubyLexerBase =>

  private val delimiters    = mutable.Stack[Int]()
  private val endTokenTypes = mutable.Stack[Int]()

  private def closingDelimiterFor(char: Int): Int = char match
    case '(' => ')'
    case '[' => ']'
    case '{' => '}'
    case '<' => '>'
    case c   => c

  private def currentOpeningDelimiter: Int = delimiters.top

  private def currentClosingDelimiter: Int = closingDelimiterFor(currentOpeningDelimiter)

  private def isOpeningDelimiter(char: Int): Boolean = char == currentOpeningDelimiter

  private def isClosingDelimiter(char: Int): Boolean = char == currentClosingDelimiter

  def pushQuotedDelimiter(char: Int): Unit = delimiters.push(char)

  def popQuotedDelimiter(): Unit = delimiters.pop()

  def pushQuotedEndTokenType(endTokenType: Int): Unit = endTokenTypes.push(endTokenType)

  def popQuotedEndTokenType(): Int = endTokenTypes.pop()

  def consumeQuotedCharAndMaybePopMode(char: Int): Unit = {
    if (isClosingDelimiter(char)) {
      popQuotedDelimiter()

      if (delimiters.isEmpty) {
        setType(endTokenTypes.pop())
        popMode()
      }
    } else if (isOpeningDelimiter(char)) {
      pushQuotedDelimiter(char)
    }
  }
}
