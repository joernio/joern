package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.parser.RubyLexer.{
  QUOTED_NON_EXPANDED_STRING_LITERAL_END,
  QUOTED_NON_EXPANDED_REGULAR_EXPRESSION_END,
  QUOTED_NON_EXPANDED_STRING_ARRAY_LITERAL_END,
  QUOTED_NON_EXPANDED_SYMBOL_ARRAY_LITERAL_END
}

import scala.collection.mutable

trait NonExpandedDelimiterHandling { this: RubyLexerBase =>

  private val delimiters   = mutable.Stack[Int]()
  private var endTokenType = 0

  private def pushNonExpandedDelimiter(char: Int): Unit = {
    delimiters.push(char)
  }

  private def popNonExpandedDelimiter(): Unit = {
    delimiters.pop()
  }

  private def isNonExpandedDelimitersStackEmpty: Boolean = {
    delimiters.isEmpty
  }

  private def isNonExpandedOpeningDelimiter(char: Int): Boolean = {
    char == currentOpeningDelimiter()
  }

  private def isNonExpandedClosingDelimiter(char: Int): Boolean = {
    char == currentClosingDelimiter()
  }

  private def currentOpeningDelimiter(): Int = {
    delimiters.top
  }

  def setNonExpandedDelimiterEndToken(endTokenType: Int): Unit = {
    this.endTokenType = endTokenType
  }

  private def getNonExpandedDelimitedStringEndToken: Int = endTokenType

  private def currentClosingDelimiter(): Int = closingDelimiterFor(currentOpeningDelimiter())

  def pushNonExpandedStringDelimiter(char: Int): Unit = {
    pushNonExpandedDelimiter(char)
    setNonExpandedDelimiterEndToken(QUOTED_NON_EXPANDED_STRING_LITERAL_END)
  }

  def pushNonExpandedRegexDelimiter(char: Int): Unit = {
    pushNonExpandedDelimiter(char)
    setNonExpandedDelimiterEndToken(QUOTED_NON_EXPANDED_REGULAR_EXPRESSION_END)
  }

  def pushNonExpandedStringArrayDelimiter(char: Int): Unit = {
    pushNonExpandedDelimiter(char)
    setNonExpandedDelimiterEndToken(QUOTED_NON_EXPANDED_STRING_ARRAY_LITERAL_END)
  }

  def pushNonExpandedSymbolArrayDelimiter(char: Int): Unit = {
    pushNonExpandedDelimiter(char)
    setNonExpandedDelimiterEndToken(QUOTED_NON_EXPANDED_SYMBOL_ARRAY_LITERAL_END)
  }

  def consumeNonExpandedCharAndMaybePopMode(char: Int): Unit = {
    if (isNonExpandedClosingDelimiter(char)) {
      popNonExpandedDelimiter()

      if (isNonExpandedDelimitersStackEmpty) {
        setType(getNonExpandedDelimitedStringEndToken)
        popMode()
      }
    } else if (isNonExpandedOpeningDelimiter(char)) {
      pushNonExpandedDelimiter(char)
    }
  }

}
