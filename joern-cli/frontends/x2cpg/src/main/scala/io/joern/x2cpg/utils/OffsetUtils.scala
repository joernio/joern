package io.joern.x2cpg.utils

import java.nio.charset.{Charset, StandardCharsets}

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

  def buildUtf8ToUtf16OffsetTable(utf8Bytes: Array[Byte]): Array[Int] = {
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

  def buildCodepointToUtf16OffsetTable(content: String): Array[Int] = {
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

  def buildIso8859ToUtf16OffsetTable(bytes: Array[Byte]): Array[Int] = {
    val table = new Array[Int](bytes.length + 1)
    var i     = 0
    while (i <= bytes.length) {
      table(i) = i
      i += 1
    }
    table
  }

  def buildByteToUtf16OffsetTable(bytes: Array[Byte], charset: Charset): Array[Int] = {
    if (charset == StandardCharsets.UTF_8 || charset.name() == "UTF-8") {
      return buildUtf8ToUtf16OffsetTable(bytes)
    }
    if (charset == StandardCharsets.ISO_8859_1 || charset.name() == "ISO-8859-1") {
      return buildIso8859ToUtf16OffsetTable(bytes)
    }
    // For non-UTF-8 charsets, decode to string and walk the UTF-16 representation to build the mapping.
    val content  = new String(bytes, charset)
    val table    = new Array[Int](bytes.length + 1)
    var byteIdx  = 0
    var utf16Idx = 0
    while (utf16Idx < content.length && byteIdx < bytes.length) {
      table(byteIdx) = utf16Idx
      val ch      = content.charAt(utf16Idx)
      val charStr = if (Character.isHighSurrogate(ch) && utf16Idx + 1 < content.length) {
        utf16Idx += 1
        new String(Array(ch, content.charAt(utf16Idx)), 0, 2)
      } else {
        String.valueOf(ch)
      }
      val charBytes = charStr.getBytes(charset).length
      var i         = 1
      while (i < charBytes && (byteIdx + i) < bytes.length) {
        table(byteIdx + i) = table(byteIdx)
        i += 1
      }
      byteIdx += charBytes
      utf16Idx += 1
    }
    // Fill any remaining positions
    while (byteIdx <= bytes.length) {
      table(byteIdx) = utf16Idx
      byteIdx += 1
    }
    table
  }
}
