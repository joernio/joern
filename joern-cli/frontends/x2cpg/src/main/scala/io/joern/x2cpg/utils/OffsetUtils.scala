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
      buildUtf8ToUtf16OffsetTable(bytes)(_)
    case OffsetSource.Bytes(_, charset) if charset == StandardCharsets.ISO_8859_1 =>
      identity
    case OffsetSource.Bytes(_, charset) =>
      logger.warn(s"Unhandled charset ${charset.name()} for offset conversion, defaulting to identity mapping")
      identity
    case OffsetSource.Codepoints(content) =>
      buildCodepointToUtf16OffsetTable(content)(_)
  }

  private def buildUtf8ToUtf16OffsetTable(utf8Bytes: Array[Byte]): Array[Int] = {
    val table    = new Array[Int](utf8Bytes.length + 1)
    var utf16Idx = 0
    var byteIdx  = 0
    while (byteIdx < utf8Bytes.length) {
      table(byteIdx) = utf16Idx
      val leadingByte = utf8Bytes(byteIdx) & 0xff
      // UTF-8 leading byte encodes sequence length: 0xxxxxxx = 1 byte (ASCII),
      // 110xxxxx = 2 bytes, 1110xxxx = 3 bytes, 11110xxx = 4 bytes.
      val seqLen = if (leadingByte < 0x80) 1 else if (leadingByte < 0xe0) 2 else if (leadingByte < 0xf0) 3 else 4
      // Fill intermediate bytes in multi-byte sequences with the same UTF-16 offset,
      // so lookups at any byte within a character return a usable value.
      var i = 1
      while (i < seqLen && (byteIdx + i) < utf8Bytes.length) {
        table(byteIdx + i) = utf16Idx
        i += 1
      }
      // 4-byte UTF-8 sequences produce 2 UTF-16 code units (a surrogate pair)
      utf16Idx += (if (seqLen == 4) 2 else 1)
      byteIdx += seqLen
    }
    table(math.min(byteIdx, utf8Bytes.length)) = utf16Idx
    table
  }

  private def buildCodepointToUtf16OffsetTable(content: String): Array[Int] = {
    val codepointCount = content.codePointCount(0, content.length)
    val table          = new Array[Int](codepointCount + 1)
    var utf16Idx       = 0
    var cpIdx          = 0
    while (cpIdx < codepointCount) {
      table(cpIdx) = utf16Idx
      val cp = content.codePointAt(utf16Idx)
      utf16Idx += Character.charCount(cp)
      cpIdx += 1
    }
    table(cpIdx) = utf16Idx
    table
  }
}
