package io.joern.rubysrc2cpg.parser

import scala.collection.mutable

trait NonExpandedDelimiterHandling { this: RubyLexerBase =>

  private val delimiters   = mutable.Stack[Int]()
  private var endTokenType = 0

  def pushNonExpandedDelimiter(char: Int): Unit = {
    delimiters.push(char)
  }

  def popNonExpandedDelimiter(): Unit = {
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
