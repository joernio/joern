package io.joern.rubysrc2cpg.parser

import scala.collection.mutable

trait RubyLexerQuotedNonExpandedStringHandling { this: RubyLexerBase =>

  private val quotedNonExpandedStringLiteralOpeningDelimiters = mutable.Stack[Int]()

  def pushQuotedNonExpandedStringDelimiter(char: Int): Unit = {
    quotedNonExpandedStringLiteralOpeningDelimiters.push(char)
  }

  def popQuotedNonExpandedStringDelimiter(): Unit = {
    quotedNonExpandedStringLiteralOpeningDelimiters.pop()
  }

  def isQuotedNonExpandedStringDelimitersEmpty: Boolean = {
    quotedNonExpandedStringLiteralOpeningDelimiters.isEmpty
  }

  def isQuotedNonExpandedStringOpeningDelimiter(char: Int): Boolean = {
    char == currentOpeningDelimiter()
  }

  def isQuotedNonExpandedStringClosingDelimiter(char: Int): Boolean = {
    char == currentClosingDelimiter()
  }

  private def currentOpeningDelimiter(): Int = {
    quotedNonExpandedStringLiteralOpeningDelimiters.top
  }

  private def currentClosingDelimiter(): Int =
    currentOpeningDelimiter() match {
      case '(' => ')'
      case '[' => ']'
      case '{' => '}'
      case '<' => '>'
      case c   => c
    }

}
