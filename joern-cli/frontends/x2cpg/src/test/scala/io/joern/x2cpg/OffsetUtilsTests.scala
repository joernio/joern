package io.joern.x2cpg

import io.joern.x2cpg.utils.OffsetUtils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.charset.StandardCharsets

class OffsetUtilsTests extends AnyWordSpec with Matchers {

  "buildUtf8ToUtf16OffsetTable" should {

    "be identity for ASCII-only content" in {
      val bytes = "hello world".getBytes(StandardCharsets.UTF_8)
      val table = OffsetUtils.buildUtf8ToUtf16OffsetTable(bytes)
      for (i <- bytes.indices) {
        table(i) shouldBe i
      }
      table(bytes.length) shouldBe bytes.length
    }

    "handle empty input" in {
      val table = OffsetUtils.buildUtf8ToUtf16OffsetTable(Array.empty)
      table.length shouldBe 1
      table(0) shouldBe 0
    }

    "correctly map 2-byte UTF-8 chars (é)" in {
      // "café" = c(1) a(1) f(1) é(2) = 5 bytes, 4 chars
      val str   = "café"
      val bytes = str.getBytes(StandardCharsets.UTF_8)
      bytes.length shouldBe 5
      str.length shouldBe 4

      val table = OffsetUtils.buildUtf8ToUtf16OffsetTable(bytes)
      // c=byte0→char0, a=byte1→char1, f=byte2→char2, é=byte3→char3, end=byte5→char4
      table(0) shouldBe 0 // 'c'
      table(1) shouldBe 1 // 'a'
      table(2) shouldBe 2 // 'f'
      table(3) shouldBe 3 // 'é' first byte
      table(4) shouldBe 3 // 'é' second byte (intermediate)
      table(5) shouldBe 4 // end of string
    }

    "correctly map 3-byte UTF-8 chars (中文)" in {
      // "中文" = 中(3) 文(3) = 6 bytes, 2 chars
      val str   = "中文"
      val bytes = str.getBytes(StandardCharsets.UTF_8)
      bytes.length shouldBe 6
      str.length shouldBe 2

      val table = OffsetUtils.buildUtf8ToUtf16OffsetTable(bytes)
      table(0) shouldBe 0 // '中' first byte
      table(3) shouldBe 1 // '文' first byte
      table(6) shouldBe 2 // end of string
    }

    "correctly map 4-byte UTF-8 chars (🎉) to 2 UTF-16 code units" in {
      // "🎉" = 4 bytes, 2 UTF-16 code units (surrogate pair)
      val str   = "🎉"
      val bytes = str.getBytes(StandardCharsets.UTF_8)
      bytes.length shouldBe 4
      str.length shouldBe 2

      val table = OffsetUtils.buildUtf8ToUtf16OffsetTable(bytes)
      table(0) shouldBe 0 // '🎉' first byte
      table(1) shouldBe 0 // intermediate
      table(2) shouldBe 0 // intermediate
      table(3) shouldBe 0 // intermediate
      table(4) shouldBe 2 // end — 2 UTF-16 code units
    }

    "correctly handle mixed content" in {
      // "A🎉B" = A(1) 🎉(4) B(1) = 6 bytes; A(1) 🎉(2) B(1) = 4 UTF-16 code units
      val str   = "A🎉B"
      val bytes = str.getBytes(StandardCharsets.UTF_8)
      bytes.length shouldBe 6
      str.length shouldBe 4

      val table = OffsetUtils.buildUtf8ToUtf16OffsetTable(bytes)
      table(0) shouldBe 0 // 'A'
      table(1) shouldBe 1 // '🎉' first byte
      table(5) shouldBe 3 // 'B'
      table(6) shouldBe 4 // end
    }

    "produce offsets that correctly index into a Java String" in {
      // End-to-end: given a source with multi-byte chars, byte offsets from a parser
      // should map to char offsets that correctly substring the Java String.
      val source = "// 🎉\nlet x = 42\n"
      val bytes  = source.getBytes(StandardCharsets.UTF_8)
      val table  = OffsetUtils.buildUtf8ToUtf16OffsetTable(bytes)

      // "42" in UTF-8 bytes starts at byte 12, ends at byte 14
      // (// =2, space=1, 🎉=4, \n=1, let=3, space=1 = byte 12)
      val byteStart = source.getBytes(StandardCharsets.UTF_8).indexOf('4'.toByte)
      val byteEnd   = byteStart + 2

      val charStart = table(byteStart)
      val charEnd   = table(byteEnd)
      source.substring(charStart, charEnd) shouldBe "42"
    }
  }

  "buildIso8859ToUtf16OffsetTable" should {

    "be identity for ASCII content" in {
      val bytes = "hello".getBytes(StandardCharsets.ISO_8859_1)
      val table = OffsetUtils.buildIso8859ToUtf16OffsetTable(bytes)
      for (i <- 0 to bytes.length) {
        table(i) shouldBe i
      }
    }

    "be identity for accented chars (ä, ö, ü)" in {
      // In ISO-8859-1, ä=0xE4, ö=0xF6, ü=0xFC — each is a single byte
      val str   = "äöü"
      val bytes = str.getBytes(StandardCharsets.ISO_8859_1)
      bytes.length shouldBe 3

      val table = OffsetUtils.buildIso8859ToUtf16OffsetTable(bytes)
      table(0) shouldBe 0 // ä
      table(1) shouldBe 1 // ö
      table(2) shouldBe 2 // ü
      table(3) shouldBe 3 // end
    }

    "produce offsets that correctly index into a Java String" in {
      // Mixed accented + ASCII: "café = 1" in ISO-8859-1
      val str   = "café = 1"
      val bytes = str.getBytes(StandardCharsets.ISO_8859_1)
      val table = OffsetUtils.buildIso8859ToUtf16OffsetTable(bytes)

      // "1" is at byte index 7 in ISO-8859-1 (c=0, a=1, f=2, é=3, space=4, ==5, space=6, 1=7)
      val content = new String(bytes, StandardCharsets.ISO_8859_1)
      content.substring(table(7), table(8)) shouldBe "1"
    }

    "handle empty input" in {
      val table = OffsetUtils.buildIso8859ToUtf16OffsetTable(Array.empty)
      table.length shouldBe 1
      table(0) shouldBe 0
    }
  }

  "buildCodepointToUtf16OffsetTable" should {

    "be identity for ASCII-only content" in {
      val table = OffsetUtils.buildCodepointToUtf16OffsetTable("hello")
      for (i <- 0 to 5) { table(i) shouldBe i }
    }

    "be identity for BMP characters (é, 中)" in {
      // "café" = 4 codepoints, 4 UTF-16 code units
      val str   = "café"
      val table = OffsetUtils.buildCodepointToUtf16OffsetTable(str)
      table(0) shouldBe 0 // c
      table(1) shouldBe 1 // a
      table(2) shouldBe 2 // f
      table(3) shouldBe 3 // é
      table(4) shouldBe 4 // end
    }

    "shift offsets for supplementary plane characters (emoji)" in {
      // "A🎉B" = 3 codepoints, but 4 UTF-16 code units (🎉 = surrogate pair)
      val str   = "A🎉B"
      val table = OffsetUtils.buildCodepointToUtf16OffsetTable(str)
      str.codePointCount(0, str.length) shouldBe 3
      str.length shouldBe 4

      table(0) shouldBe 0 // A
      table(1) shouldBe 1 // 🎉 starts at char index 1
      table(2) shouldBe 3 // B starts at char index 3 (after surrogate pair)
      table(3) shouldBe 4 // end
    }

    "produce offsets that correctly index into a Java String" in {
      val source = "# 🎉\nx = 42\n"
      val table  = OffsetUtils.buildCodepointToUtf16OffsetTable(source)

      // "42" in codepoints: # (1) space(1) 🎉(1) \n(1) x(1) space(1) =(1) space(1) = codepoint 8
      val cpStart = source.codePointCount(0, source.indexOf("42"))
      val cpEnd   = cpStart + 2

      source.substring(table(cpStart), table(cpEnd)) shouldBe "42"
    }
  }

}
