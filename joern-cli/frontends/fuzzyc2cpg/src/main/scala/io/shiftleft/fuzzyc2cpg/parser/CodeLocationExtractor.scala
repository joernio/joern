package io.shiftleft.fuzzyc2cpg.parser

import io.shiftleft.fuzzyc2cpg.ast.CodeLocation
import org.antlr.v4.runtime.ParserRuleContext

object CodeLocationExtractor {

  def extractFromContext(ctx: ParserRuleContext): CodeLocation = {
    val startLine = Some(ctx.start.getLine)
    val startPos = Some(ctx.start.getCharPositionInLine)
    val startIndex = Some(ctx.start.getStartIndex)
    val endLine = Option(ctx.stop).map(_.getLine)
    val endIndex = Option(ctx.stop).map(_.getStopIndex)
    val endPos = Option(ctx.stop).map(_.getCharPositionInLine)
    CodeLocation(startLine, startPos, startIndex, endIndex, endLine, endPos)
  }

}
