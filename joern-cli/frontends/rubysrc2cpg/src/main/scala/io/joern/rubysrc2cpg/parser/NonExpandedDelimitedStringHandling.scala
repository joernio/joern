package io.joern.rubysrc2cpg.parser

import scala.collection.mutable

trait NonExpandedDelimitedStringHandling { this: RubyLexerBase =>

  private val delimiters   = mutable.Stack[Int]()
  private var endTokenType = 0

  def pushNonExpandedDelimiter(char: Int): Unit = {
    delimiters.push(char)
  }

  def popNonExpandedDelimiter(): Unit = {
    delimiters.pop()
  }

  def isNonExpandedDelimitersStackEmpty: Boolean = {
    delimiters.isEmpty
  }

  def isNonExpandedOpeningDelimiter(char: Int): Boolean = {
    char == currentOpeningDelimiter()
  }

  def isNonExpandedClosingDelimiter(char: Int): Boolean = {
    char == currentClosingDelimiter()
  }

  def isNonExpandedDelimiter(char: Int): Boolean = {
    isNonExpandedOpeningDelimiter(char) || isNonExpandedClosingDelimiter(char)
  }

  private def currentOpeningDelimiter(): Int = {
    delimiters.top
  }

  def setNonExpandedDelimitedStringEndToken(endTokenType: Int): Unit = {
    this.endTokenType = endTokenType
  }

  def getNonExpandedDelimitedStringEndToken: Int = endTokenType

  private def currentClosingDelimiter(): Int =
    currentOpeningDelimiter() match {
      case '(' => ')'
      case '[' => ']'
      case '{' => '}'
      case '<' => '>'
      case c   => c
    }

}
