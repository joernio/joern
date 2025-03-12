package io.joern.x2cpg.utils

object OffsetUtils {
  def getLineOffsetTable(fileContent: Option[String]): Array[Int] = {
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

  def coordinatesToOffset(
    lineOffsetTable: Array[Int],
    startLine: Int,
    startColumn: Int,
    endLine: Int,
    endColumn: Int
  ): (Int, Int) = {
    val offset    = lineOffsetTable(startLine) + startColumn
    val offsetEnd = lineOffsetTable(endLine) + endColumn + 1
    (offset, offsetEnd)
  }
}
