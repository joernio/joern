/** originally from amm/util/src/main/scala/ammonite/util/PositionOffsetConversion.scala */
package ammonite.foo

import scala.collection.mutable

/** originally from amm/util/src/main/scala/ammonite/util/Position.scala */
final case class Position(line: Int, char: Int)

object PositionOffsetConversion {

  private def lineStartIndices(content: String): Array[Int] = {

    val content0 = content.toCharArray

    // adapted from scala/scala SourceFile.scala

    val length = content0.length
    val CR = '\r'
    val LF = '\n'

    def charAtIsEOL(idx: Int)(p: Char => Boolean) = {
      // don't identify the CR in CR LF as a line break, since LF will do.
      def notCRLF0 =
        content0(idx) != CR ||
          !content0.isDefinedAt(idx + 1) ||
          content0(idx + 1) != LF

      idx < length && notCRLF0 && p(content0(idx))
    }

    def isAtEndOfLine(idx: Int) = charAtIsEOL(idx) {
      case CR | LF => true
      case _       => false
    }

    val buf = new mutable.ArrayBuffer[Int]
    buf += 0
    for (i <- 0 until content0.length if isAtEndOfLine(i))
      buf += i + 1
    buf.toArray
  }

  /** Converts a character offset from beginning of `content` to a `Position` */
  def offsetToPos(content: String): Int => Position = {

    val lineStartIndices0 = lineStartIndices(content)

    def offsetToLine(offset: Int): Int = {

      assert(lineStartIndices0.nonEmpty)

      if (offset >= lineStartIndices0.last) lineStartIndices0.length - 1
      else {
        def find(a: Int, b: Int): Int =
          if (a + 1 >= b) a
          else {
            val c = (a + b) / 2
            val idx = lineStartIndices0(c)
            if (idx == offset) c
            else if (idx < offset) find(c, b)
            else find(a, c)
          }
        find(0, lineStartIndices0.length - 1)
      }
    }

    offset =>
      assert(offset >= 0)
      assert(offset <= content.length)
      val line = offsetToLine(offset)
      Position(line, offset - lineStartIndices0(line))
  }

  private def indicesOf(input: String, elem: String): Array[Int] = {
    val b = new mutable.ArrayBuffer[Int]
    var idx = 0
    while (idx >= 0 && idx < input.length) {
      val nextIdx = input.indexOf(elem, idx)
      if (nextIdx >= 0) {
        b += nextIdx
        idx = nextIdx + 1
      } else
        idx = -1
    }
    b.toArray
  }

  /** Returns the start and end indices of sections delimited by `start` and `end` in `input` */
  def sections(input: String, start: String, end: String): Array[(Int, Int)] = {
    val b = new mutable.ArrayBuffer[(Int, Int)]
    var idx = 0
    while (idx < input.length) {
      val startIdx = input.indexOf(start, idx)
      if (startIdx >= 0) {
        val endIdx = input.indexOf(end, startIdx + start.length)
        if (endIdx >= 0) {
          idx = endIdx + end.length
          b.append((startIdx, idx))
        } else
          idx = input.length
      } else
        idx = input.length
    }
    b.toArray
  }

  /** Removes sections delimited by `ignoreSections` from offset `idx` */
  def extraOffset(ignoreSections: Seq[(Int, Int)], idx: Int): Int =
    ignoreSections
      .iterator
      .takeWhile(_._2 <= idx)
      .map {
        case (start, end) =>
          end - start
      }
      .sum

  private val firstLineWrapperPrefix = "/*<script>*/"

  def scalaPosToScPos(
    scCode: String,
    startLineInSc: Int,
    startPosInSc: Int,
    scalaCode: String,
    startOffsetInScala: Int
  ): (Int, Int) => Option[(Int, Int)] = {
    val lineStartIndicesScala = lineStartIndices(scalaCode)
    val offsetToPosScala = offsetToPos(scalaCode)
    val generatedCodeIndicesScala =
      sections(
        scalaCode,
        "/*<amm>*/", "/*</amm>*/"
      ).toVector
    val startPosInScala = offsetToPosScala(startOffsetInScala)

    val lineStartIndicesSc = lineStartIndices(scCode)
    val lastLineLengthSc = scCode.length - lineStartIndicesSc.last

    def valid(lineCharSc: (Int, Int)): Boolean = {
      val (lineSc, charSc) = lineCharSc
      val validLine = lineSc >= 0 && lineSc < lineStartIndicesSc.length
      val validChar = charSc >= 0 && (
        lineSc != lineStartIndicesSc.length - 1 ||
          charSc <= lastLineLengthSc
      )
      validLine && validChar
    }

    (lineScala: Int, charScala: Int) =>
      val lineStartScala = lineStartIndicesScala(lineScala)
      val lineEndScala =
        if (lineScala < lineStartIndicesScala.length - 1)
          lineStartIndicesScala(lineScala + 1)
        else
          scalaCode.length
      val extraCharOffsetInLineScala = extraOffset(
        generatedCodeIndicesScala
          .filter(t => t._1 >= lineStartScala && t._2 <= lineEndScala),
        lineStartScala + charScala
      )
      val lineSc = lineScala - startPosInScala.line + startLineInSc

      def lineStartsWithWrapperPrefix: Boolean =
        scalaCode.regionMatches(
          lineStartScala,
          firstLineWrapperPrefix,
          0,
          firstLineWrapperPrefix.length
        )
      val firstScLineCharOffsetInLineScala =
        if (lineSc == 0 && lineStartsWithWrapperPrefix)
          firstLineWrapperPrefix.length
        else
          0

      val charSc = charScala +
        -extraCharOffsetInLineScala +
        -firstScLineCharOffsetInLineScala +
        startPosInSc

      Some((lineSc, charSc))
        .filter(valid)
  }

}
