package io.joern.x2cpg.utils

import org.slf4j.LoggerFactory

import java.nio.charset.{Charset, StandardCharsets}

object OffsetUtils {

  private val logger = LoggerFactory.getLogger(getClass)

  sealed trait OffsetSource
  object OffsetSource {

    /** Parser offsets are byte offsets into `bytes`, encoded using `charset`. */
    final case class Bytes(bytes: Array[Byte], charset: Charset) extends OffsetSource

    /** Parser offsets are Unicode codepoint offsets into `content`. */
    final case class Codepoints(content: String) extends OffsetSource
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

  def buildOffsetConverter(source: OffsetSource): Int => Int = source match {
    case OffsetSource.Bytes(bytes, charset) if charset == StandardCharsets.UTF_8 =>
      buildUtf8ToUtf16Converter(bytes)
    case OffsetSource.Bytes(_, charset) if charset == StandardCharsets.ISO_8859_1 =>
      identity
    case OffsetSource.Bytes(_, charset) =>
      logger.warn(s"Unhandled charset ${charset.name()} for offset conversion, defaulting to identity mapping")
      identity
    case OffsetSource.Codepoints(content) =>
      buildCodepointToUtf16Converter(content)
  }

  private final class SparseOffsetConverter(keys: Array[Int], vals: Array[Int], isMultiUnitSpan: Array[Boolean])
      extends (Int => Int) {
    def apply(query: Int): Int = {
      val idx = java.util.Arrays.binarySearch(keys, query)
      if (idx >= 0) vals(idx)
      else {
        // Not found: binarySearch returns -(insertionPoint) - 1, where insertionPoint is the index of
        // the first breakpoint greater than `query`. So `j` (insertionPoint - 1) is the index of the
        // last breakpoint at or before `query` — the one whose rule still governs it.
        val j = -(idx + 1) - 1
        if (j < 0) query
        else vals(j) + (if (isMultiUnitSpan(j)) 0 else query - keys(j))
      }
    }
  }

  private class BreakpointBuilder {
    private val keys            = collection.mutable.ArrayBuilder.make[Int]
    private val vals            = collection.mutable.ArrayBuilder.make[Int]
    private val isMultiUnitSpan = collection.mutable.ArrayBuilder.make[Boolean]

    // A multi-unit span is where several source units (bytes/codepoints) collapse to a single UTF-16
    // offset, e.g. the 2-4 UTF-8 bytes of one character all mapping to the same UTF-16 code unit.
    def add(key: Int, value: Int, isMultiUnitSpan: Boolean): Unit = {
      keys.addOne(key); vals.addOne(value); this.isMultiUnitSpan.addOne(isMultiUnitSpan)
    }

    def build(): Int => Int = {
      val keysArr = keys.result()
      if (keysArr.isEmpty) identity
      else new SparseOffsetConverter(keysArr, vals.result(), isMultiUnitSpan.result())
    }
  }

  /** Walks source positions `0` until `totalUnits`, using `nextChar(pos, utf16Idx)` to determine how
    * many source units the character starting at `pos` consumes (`unitLen`) and how many UTF-16 units
    * it produces (`utf16Increment`), and builds a sparse offset converter from the result.
    */
  private def buildSparseConverter(totalUnits: Int)(nextChar: (Int, Int) => (Int, Int)): Int => Int = {
    val breakpoints = new BreakpointBuilder
    var pos          = 0
    var utf16Idx     = 0
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
