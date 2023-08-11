package io.joern.rubysrc2cpg.parser

import scala.collection.mutable

trait ExpandedDelimiterHandling { this: RubyLexerBase =>

  private val delimiters    = mutable.Stack[Int]()
  private val endTokenTypes = mutable.Stack[Int]()

  def pushExpandedDelimiter(char: Int): Unit = {
    delimiters.push(char)
  }

  def popExpandedDelimiter(): Unit = {
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

  def pushExpandedDelimiterEndToken(endTokenType: Int): Unit = {
    endTokenTypes.push(endTokenType)
  }

  def popExpandedDelimiterEndToken(): Int = {
    endTokenTypes.pop()
  }

  private def currentClosingDelimiter(): Int = closingDelimiterFor(currentOpeningDelimiter())

  def consumeExpandedCharAndMaybePopMode(char: Int): Unit = {
    if (isExpandedClosingDelimiter(char)) {
      popExpandedDelimiter()

      if (isExpandedDelimitersStackEmpty) {
        setType(popExpandedDelimiterEndToken())
        popMode()
      }
    } else if (isExpandedOpeningDelimiter(char)) {
      pushExpandedDelimiter(char)
    }
  }

}
