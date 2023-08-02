package io.joern.rubysrc2cpg.parser

import scala.collection.mutable

trait RubyLexerQuotedNonExpandedRegularExpressionHandling { this: RubyLexerBase =>

  private val quotedNonExpandedRegularExpressionOpeningDelimiters = mutable.Stack[Int]()

  def pushQuotedNonExpandedRegularExpressionDelimiter(char: Int): Unit = {
    quotedNonExpandedRegularExpressionOpeningDelimiters.push(char)
  }

  def popQuotedNonExpandedRegularExpressionDelimiter(): Unit = {
    quotedNonExpandedRegularExpressionOpeningDelimiters.pop()
  }

  def isQuotedNonExpandedRegularExpressionDelimitersEmpty: Boolean = {
    quotedNonExpandedRegularExpressionOpeningDelimiters.isEmpty
  }

  def isQuotedNonExpandedRegularExpressionOpeningDelimiter(char: Int): Boolean = {
    char == currentOpeningDelimiter()
  }

  def isQuotedNonExpandedRegularExpressionClosingDelimiter(char: Int): Boolean = {
    char == currentClosingDelimiter()
  }

  private def currentOpeningDelimiter(): Int = {
    quotedNonExpandedRegularExpressionOpeningDelimiters.top
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
