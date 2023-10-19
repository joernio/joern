package io.joern.x2cpg.utils

import io.joern.x2cpg.AstNodeBuilder.ZeroIndexedCoordinates

object OffsetUtils {
  def getLineOffsetIndex(fileContent: Option[String]): Array[Int] = {
    fileContent
      .map { content =>
        var totalCharsCounted = 0
        content.linesWithSeparators.map { line =>
          val lineStartOffset = totalCharsCounted
          totalCharsCounted += line.length()
          lineStartOffset
        }.toArray
      }
      .getOrElse(Array.empty)
  }

  def coordinatesToOffset(lineOffsetIndex: Array[Int], coordinates: ZeroIndexedCoordinates): Option[(Int, Int)] = {
    coordinates.match {
      case ZeroIndexedCoordinates(startLine, startColumn, endLine, endColumn)
          if endLine <= lineOffsetIndex.length - 1 =>
        val offset    = lineOffsetIndex(startLine) + startColumn
        val offsetEnd = lineOffsetIndex(endLine) + endColumn + 1
        Some((offset, offsetEnd))

      case _ => None
    }
  }
}
