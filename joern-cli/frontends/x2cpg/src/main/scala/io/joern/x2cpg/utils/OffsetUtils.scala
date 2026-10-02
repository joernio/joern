package io.joern.x2cpg.utils

import org.slf4j.LoggerFactory

import java.nio.charset.{Charset, StandardCharsets}
import scala.collection.Searching.*

object OffsetUtils {

  private val logger = LoggerFactory.getLogger(getClass)

  sealed trait OffsetSourceType
  object OffsetSourceType {

    /** Parser offsets are byte offsets into `bytes`, encoded using `charset`. */
    final case class Bytes(bytes: Array[Byte], charset: Charset) extends OffsetSourceType

    /** Parser offsets are Unicode codepoint offsets into `content`. */
    final case class Codepoints(content: String) extends OffsetSourceType
  }

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

  def buildOffsetConverter(source: OffsetSourceType): Int => Int = source match {
    case OffsetSourceType.Bytes(bytes, charset) if charset == StandardCharsets.UTF_8 =>
      buildUtf8ToUtf16Converter(bytes)
    case OffsetSourceType.Bytes(_, charset) if charset == StandardCharsets.ISO_8859_1 =>
      identity
    case OffsetSourceType.Bytes(_, charset) =>
      logger.warn(s"Unhandled charset ${charset.name()} for offset conversion, defaulting to identity mapping")
      identity
    case OffsetSourceType.Codepoints(content) =>
      buildCodepointToUtf16Converter(content)
  }

  private final class SparseOffsetConverter(indices: Array[Int], offsets: Array[Int], isMultiUnitSpan: Array[Boolean])
      extends (Int => Int) {
    def apply(query: Int): Int = {
      indices.search(query) match {
        case Found(idx)                     => offsets(idx)
        case InsertionPoint(insertionPoint) =>
          // `j` is the last breakpoint at or before `query` — the one whose rule still governs it.
          val j = insertionPoint - 1
          if (j < 0) {
            // query precedes any recorded breakpoint, so no divergence from identity has happened yet
            query
          } else if (isMultiUnitSpan(j)) {
            offsets(j)
          } else {
            offsets(j) + (query - indices(j))
          }
      }
    }
  }

  private class BreakpointBuilder {
    private val indices         = collection.mutable.ArrayBuilder.make[Int]
    private val offsets         = collection.mutable.ArrayBuilder.make[Int]
    private val isMultiUnitSpan = collection.mutable.ArrayBuilder.make[Boolean]

    // A multi-unit span is where several source units (bytes/codepoints) collapse to a single UTF-16
    // offset, e.g. the 2-4 UTF-8 bytes of one character all mapping to the same UTF-16 code unit.
    def add(index: Int, offset: Int, isMultiUnitSpan: Boolean): Unit = {
      indices.addOne(index); offsets.addOne(offset); this.isMultiUnitSpan.addOne(isMultiUnitSpan)
    }

    def build(): Int => Int = {
      val indicesArr = indices.result()
      if (indicesArr.isEmpty) identity
      else new SparseOffsetConverter(indicesArr, offsets.result(), isMultiUnitSpan.result())
    }
  }

  /** Walks source positions `0` until `totalUnits`, using `nextChar(pos, utf16Idx)` to determine how many source units
    * the character starting at `pos` consumes (`unitLen`) and how many UTF-16 units it produces (`utf16Increment`), and
    * builds a sparse offset converter from the result.
    */
  private def buildSparseConverter(totalUnits: Int)(nextChar: (Int, Int) => (Int, Int)): Int => Int = {
    val breakpoints = new BreakpointBuilder
    var pos         = 0
    var utf16Idx    = 0
    while (pos < totalUnits) {
      val (unitLen, utf16Increment) = nextChar(pos, utf16Idx)
      if (unitLen == 1 && utf16Increment == 1) {
        pos += 1
        utf16Idx += 1
      } else {
        breakpoints.add(pos, utf16Idx, isMultiUnitSpan = true)
        utf16Idx += utf16Increment
        pos = math.min(pos + unitLen, totalUnits)
        breakpoints.add(pos, utf16Idx, isMultiUnitSpan = false)
      }
    }
    breakpoints.build()
  }

  private def buildUtf8ToUtf16Converter(utf8Bytes: Array[Byte]): Int => Int =
    buildSparseConverter(utf8Bytes.length) { (byteIdx, _) =>
      val leadingByte = utf8Bytes(byteIdx) & 0xff
      // UTF-8 leading byte encodes sequence length: 0xxxxxxx = 1 byte (ASCII),
      // 110xxxxx = 2 bytes, 1110xxxx = 3 bytes, 11110xxx = 4 bytes.
      val seqLen = if (leadingByte < 0x80) 1 else if (leadingByte < 0xe0) 2 else if (leadingByte < 0xf0) 3 else 4
      // 4-byte UTF-8 sequences produce 2 UTF-16 code units (a surrogate pair)
      (seqLen, if (seqLen == 4) 2 else 1)
    }

  private def buildCodepointToUtf16Converter(content: String): Int => Int = {
    val codepointCount = content.codePointCount(0, content.length)
    buildSparseConverter(codepointCount) { (_, utf16Idx) =>
      val cp = content.codePointAt(utf16Idx)
      // One codepoint consumed; charCount is 2 for a supplementary-plane codepoint (surrogate pair).
      (1, Character.charCount(cp))
    }
  }
}
