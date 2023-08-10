package io.joern.rubysrc2cpg.parser

import scala.collection.mutable

trait ExpandedDelimiterHandling { this: RubyLexerBase =>

  private val delimiters    = mutable.Stack[Int]()
  private val endTokenTypes = mutable.Stack[Int]()

  private def pushExpandedStringDelimiter(char: Int): Unit = {
    delimiters.push(char)
  }

  private def popExpandedStringDelimiter(): Unit = {
    delimiters.pop()
  }

  private def isExpandedDelimitersStackEmpty: Boolean = {
    delimiters.isEmpty
  }

  private def isExpandedOpeningDelimiter(char: Int): Boolean = {
    char == currentOpeningDelimiter()
  }

  private def isExpandedClosingDelimiter(char: Int): Boolean = {
    char == currentClosingDelimiter()
  }

  private def currentOpeningDelimiter(): Int = {
    delimiters.top
  }

  private def pushExpandedDelimiterEndToken(endTokenType: Int): Unit = {
    endTokenTypes.push(endTokenType)
  }

  private def popExpandedDelimiterEndToken(): Int = {
    endTokenTypes.pop()
  }

  private def currentClosingDelimiter(): Int = closingDelimiterFor(currentOpeningDelimiter())

  def pushExpandedQuotedStringDelimiter(char: Int): Unit = {
    pushExpandedStringDelimiter(char)
    pushExpandedDelimiterEndToken(RubyLexer.QUOTED_EXPANDED_STRING_LITERAL_END)
  }

  def consumeExpandedCharAndMaybePopMode(char: Int): Unit = {
    if (isExpandedClosingDelimiter(char)) {
      popExpandedStringDelimiter()

      if (isExpandedDelimitersStackEmpty) {
        setType(popExpandedDelimiterEndToken())
        popMode()
      }
    } else if (isExpandedOpeningDelimiter(char)) {
      pushExpandedQuotedStringDelimiter(char)
    }
  }

}
