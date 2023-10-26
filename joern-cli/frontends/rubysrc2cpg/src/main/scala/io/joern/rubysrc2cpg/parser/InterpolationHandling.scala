package io.joern.rubysrc2cpg.parser

import scala.collection.mutable

trait InterpolationHandling { this: RubyLexerBase =>

  private val interpolationEndTokenType = mutable.Stack[Int]()

  def pushInterpolationEndTokenType(endTokenType: Int): Unit = {
    interpolationEndTokenType.push(endTokenType)
  }

  def popInterpolationEndTokenType(): Int = {
    interpolationEndTokenType.pop()
  }

  def isEndOfInterpolation: Boolean = {
    interpolationEndTokenType.nonEmpty
  }

}
