package io.joern.rubysrc2cpg.parser

import scala.collection.mutable

// TODO: Very similar to `RubyLexerQuotedNonExpandedStringHandling`. Consider refactoring them.
trait RubyLexerQuotedNonExpandedStringArrayHandling { this: RubyLexerBase =>

  private val quotedNonExpandedStringArrayLiteralOpeningDelimiters = mutable.Stack[Int]()

  def pushQuotedNonExpandedStringArrayDelimiter(char: Int): Unit = {
    quotedNonExpandedStringArrayLiteralOpeningDelimiters.push(char)
  }

  def popQuotedNonExpandedStringArrayDelimiter(): Unit = {
    quotedNonExpandedStringArrayLiteralOpeningDelimiters.pop()
  }

  def isQuotedNonExpandedStringArrayDelimitersEmpty: Boolean = {
    quotedNonExpandedStringArrayLiteralOpeningDelimiters.isEmpty
  }

  def isQuotedNonExpandedStringArrayOpeningDelimiter(char: Int): Boolean = {
    char == currentOpeningDelimiter()
  }

  def isQuotedNonExpandedStringArrayClosingDelimiter(char: Int): Boolean = {
    char == currentClosingDelimiter()
  }

  private def currentOpeningDelimiter(): Int = {
    quotedNonExpandedStringArrayLiteralOpeningDelimiters.top
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
