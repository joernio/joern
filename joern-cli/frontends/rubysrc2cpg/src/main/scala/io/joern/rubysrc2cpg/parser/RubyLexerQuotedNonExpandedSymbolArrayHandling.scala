package io.joern.rubysrc2cpg.parser

import scala.collection.mutable

// TODO: Refactor with `RubyLexerQuotedNonExpandedStringArrayHandling` in mind.
trait RubyLexerQuotedNonExpandedSymbolArrayHandling { this: RubyLexerBase =>

  private val quotedNonExpandedSymbolArrayLiteralOpeningDelimiters = mutable.Stack[Int]()

  def pushQuotedNonExpandedSymbolArrayDelimiter(char: Int): Unit = {
    quotedNonExpandedSymbolArrayLiteralOpeningDelimiters.push(char)
  }

  def popQuotedNonExpandedSymbolArrayDelimiter(): Unit = {
    quotedNonExpandedSymbolArrayLiteralOpeningDelimiters.pop()
  }

  def isQuotedNonExpandedSymbolArrayDelimitersEmpty: Boolean = {
    quotedNonExpandedSymbolArrayLiteralOpeningDelimiters.isEmpty
  }

  def isQuotedNonExpandedSymbolArrayOpeningDelimiter(char: Int): Boolean = {
    char == currentOpeningDelimiter()
  }

  def isQuotedNonExpandedSymbolArrayClosingDelimiter(char: Int): Boolean = {
    char == currentClosingDelimiter()
  }

  private def currentOpeningDelimiter(): Int = {
    quotedNonExpandedSymbolArrayLiteralOpeningDelimiters.top
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
