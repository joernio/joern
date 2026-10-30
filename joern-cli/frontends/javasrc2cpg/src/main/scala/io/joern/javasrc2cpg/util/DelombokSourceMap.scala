package io.joern.javasrc2cpg.util

import scala.util.matching.Regex

/** Maps coordinates in delomboked Java back to the original Lombok source file.
  *
  * Delombok pretty-prints expanded Java; JavaParser therefore reports positions in that text. This aligner matches
  * stable line keys between the two files and projects delomboked line/column pairs onto the original file. Lines that
  * exist only after delombok (generated members, loggers) collapse onto the lombok annotation line that was removed.
  *
  * If a user statement is rewritten onto unrelated text, it is treated as deleted plus generated and anchored to the
  * previous match (ponytail: line-key walk, not a full token lexer).
  */
sealed trait DelombokSourceMap {

  def translateLine(line: Int): Int

  def translateColumn(line: Int, column: Int): Int

  final def translate(line: Int, column: Int): (Int, Int) =
    (translateLine(line), translateColumn(line, column))
}

object DelombokSourceMap {

  val Identity: DelombokSourceMap = IdentityMap

  def build(original: String, delomboked: String): Option[DelombokSourceMap] = {
    val origLines  = normalize(original).split("\n", -1)
    val delomLines = normalize(delomboked).split("\n", -1)
    if (origLines.sameElements(delomLines)) {
      Some(Identity)
    } else {
      Mapped.build(origLines, delomLines)
    }
  }

  private def normalize(text: String): String =
    text.replaceAll("\r\n", "\n")

  private object IdentityMap extends DelombokSourceMap {
    override def translateLine(line: Int): Int                = line
    override def translateColumn(line: Int, column: Int): Int = column
  }

  private final case class Entry(originalLine: Int, columnDelta: Int, generated: Boolean, donorColumn: Int)

  private final class Mapped(entries: Array[Entry], originalLineCount: Int) extends DelombokSourceMap {

    override def translateLine(line: Int): Int = {
      val entry = entryFor(line)
      math.min(math.max(entry.originalLine, 1), originalLineCount)
    }

    override def translateColumn(line: Int, column: Int): Int = {
      val entry = entryFor(line)
      if (entry.generated) entry.donorColumn
      else math.max(1, column + entry.columnDelta)
    }

    private def entryFor(line: Int): Entry = {
      val index = math.min(math.max(line - 1, 0), entries.length - 1)
      entries(index)
    }
  }

  private object Mapped {

    private val AnnotationPrefix: Regex = """^(@\w+(?:\([^)]*\))?\s*)+""".r
    private val Whitespace: Regex       = """\s+""".r

    def build(origLines: Array[String], delomLines: Array[String]): Option[DelombokSourceMap] = {
      if (delomLines.isEmpty) {
        Some(Identity)
      } else {
        val entries                   = Array.fill(delomLines.length)(Entry(1, 0, generated = false, donorColumn = 1))
        var donor: Option[(Int, Int)] = None
        var lombokDonor: Option[(Int, Int)] = None
        var lastMatchedOrig: Int            = -1
        var prevOrigIndex                   = 0
        var prevDelomIndex                  = 0
        var success                         = true

        def runGap(origTo: Int, delomTo: Int): Unit = {
          if (!success) {
            ()
          } else {
            alignGap(
              origLines,
              delomLines,
              prevOrigIndex,
              origTo,
              prevDelomIndex,
              delomTo,
              donor,
              lombokDonor,
              lastMatchedOrig,
              entries
            ) match {
              case None         => success = false
              case Some(result) =>
                donor = result.donor
                lombokDonor = result.lombokDonor
                lastMatchedOrig = result.lastMatchedOrig
            }
          }
        }

        def mapAnchor(origIndex: Int, delomIndex: Int): Unit = {
          if (!success) {
            ()
          } else {
            val key = lineKey(origLines(origIndex))
            if (key.isEmpty || origIndex <= lastMatchedOrig) {
              success = false
            } else {
              val columnDelta = keyColumn(origLines(origIndex), key) - keyColumn(delomLines(delomIndex), key)
              entries(delomIndex) = Entry(origIndex + 1, columnDelta, generated = false, donorColumn = 1)
              donor = annotationDonor(origLines(origIndex), origIndex + 1).orElse(donor)
              lombokDonor = lombokAnnotationDonor(origLines(origIndex), origIndex + 1).orElse(lombokDonor)
              lastMatchedOrig = origIndex
              prevOrigIndex = origIndex + 1
              prevDelomIndex = delomIndex + 1
            }
          }
        }

        patienceAnchors(origLines, delomLines).foreach { case (origIndex, delomIndex) =>
          runGap(origIndex, delomIndex)
          mapAnchor(origIndex, delomIndex)
        }
        runGap(origLines.length, delomLines.length)

        if (!success) {
          None
        } else {
          propagateBlankLines(delomLines, entries)
          Some(new Mapped(entries, origLines.length))
        }
      }
    }

    private final case class GapResult(donor: Option[(Int, Int)], lombokDonor: Option[(Int, Int)], lastMatchedOrig: Int)

    private def alignGap(
      origLines: Array[String],
      delomLines: Array[String],
      origFrom: Int,
      origTo: Int,
      delomFrom: Int,
      delomTo: Int,
      initialDonor: Option[(Int, Int)],
      initialLombokDonor: Option[(Int, Int)],
      initialLastMatchedOrig: Int,
      entries: Array[Entry]
    ): Option[GapResult] = {
      var donor           = initialDonor
      var lombokDonor     = initialLombokDonor
      var lastMatchedOrig = initialLastMatchedOrig
      var origIndex       = origFrom
      var delomIndex      = delomFrom
      var failed          = false

      while (!failed && (origIndex < origTo || delomIndex < delomTo)) {
        val madeProgress =
          if (origIndex < origTo && delomIndex < delomTo) {
            val origKey  = lineKey(origLines(origIndex))
            val delomKey = lineKey(delomLines(delomIndex))
            if (origKey.nonEmpty && origKey == delomKey) {
              if (origIndex <= lastMatchedOrig) {
                failed = true
                false
              } else {
                val columnDelta =
                  keyColumn(origLines(origIndex), origKey) - keyColumn(delomLines(delomIndex), delomKey)
                entries(delomIndex) = Entry(origIndex + 1, columnDelta, generated = false, donorColumn = 1)
                donor = annotationDonor(origLines(origIndex), origIndex + 1).orElse(donor)
                lombokDonor = lombokAnnotationDonor(origLines(origIndex), origIndex + 1).orElse(lombokDonor)
                lastMatchedOrig = origIndex
                origIndex += 1
                delomIndex += 1
                true
              }
            } else if (origKey.isEmpty) {
              donor = annotationDonor(origLines(origIndex), origIndex + 1).orElse(donor)
              lombokDonor = lombokAnnotationDonor(origLines(origIndex), origIndex + 1).orElse(lombokDonor)
              origIndex += 1
              true
            } else if (delomKey.isEmpty && delomLines(delomIndex).trim.isEmpty) {
              entries(delomIndex) = entries(math.max(delomIndex - 1, 0))
              delomIndex += 1
              true
            } else {
              assignGenerated(delomIndex, entries, lombokDonor.orElse(donor), lastMatchedOrig)
              delomIndex += 1
              true
            }
          } else if (delomIndex < delomTo) {
            assignGenerated(delomIndex, entries, lombokDonor.orElse(donor), lastMatchedOrig)
            delomIndex += 1
            true
          } else if (origIndex < origTo) {
            donor = annotationDonor(origLines(origIndex), origIndex + 1).orElse(donor)
            lombokDonor = lombokAnnotationDonor(origLines(origIndex), origIndex + 1).orElse(lombokDonor)
            origIndex += 1
            true
          } else {
            false
          }

        if (!madeProgress) {
          failed = true
        }
      }

      if (failed) None else Some(GapResult(donor, lombokDonor, lastMatchedOrig))
    }

    private def assignGenerated(
      delomIndex: Int,
      entries: Array[Entry],
      donor: Option[(Int, Int)],
      lastMatchedOrig: Int
    ): Unit = {
      val fallbackLine             = math.max(lastMatchedOrig, 0) + 1
      val (donorLine, donorColumn) = donor.getOrElse((fallbackLine, 1))
      entries(delomIndex) = Entry(donorLine, columnDelta = 0, generated = true, donorColumn = donorColumn)
    }

    private def propagateBlankLines(delomLines: Array[String], entries: Array[Entry]): Unit = {
      var delomIndex = 1
      while (delomIndex < entries.length) {
        if (delomLines(delomIndex).trim.isEmpty) {
          entries(delomIndex) = entries(delomIndex - 1)
        }
        delomIndex += 1
      }
    }

    private def lineKey(line: String): String = {
      val trimmed = line.trim
      if (trimmed.isEmpty) {
        ""
      } else {
        val withoutAnnotations = AnnotationPrefix.replaceAllIn(trimmed, "")
        Whitespace.replaceAllIn(withoutAnnotations.trim, " ")
      }
    }

    private def keyColumn(line: String, key: String): Int = {
      val trimmed          = line.trim
      val afterAnnotations = AnnotationPrefix.replaceAllIn(trimmed, "").trim
      val keyStartInAfter  = afterAnnotations.indexOf(key)
      if (keyStartInAfter < 0) {
        1
      } else {
        val prefix = trimmed.substring(0, trimmed.length - afterAnnotations.length + keyStartInAfter)
        prefix.length + 1
      }
    }

    private def annotationDonor(line: String, oneBasedLine: Int): Option[(Int, Int)] = {
      val atColumn = line.indexOf('@')
      if (atColumn >= 0) Some((oneBasedLine, atColumn + 1)) else None
    }

    private def lombokAnnotationDonor(line: String, oneBasedLine: Int): Option[(Int, Int)] = {
      val trimmed = line.trim
      if (trimmed.startsWith("@Slf4j") || trimmed.startsWith("@Getter") || trimmed.startsWith("@Data")) {
        annotationDonor(line, oneBasedLine)
      } else {
        None
      }
    }

    private def uniqueKeyToIndex(lines: Array[String]): Map[String, Int] = {
      lines.indices
        .map(index => (lineKey(lines(index)), index))
        .filter { case (key, _) => key.nonEmpty }
        .groupBy(_._1)
        .collect { case (key, occurrences) if occurrences.size == 1 => key -> occurrences.head._2 }
    }

    private def patienceAnchors(origLines: Array[String], delomLines: Array[String]): List[(Int, Int)] = {
      val origByKey  = uniqueKeyToIndex(origLines)
      val delomByKey = uniqueKeyToIndex(delomLines)
      val sharedKeys = origByKey.keySet.intersect(delomByKey.keySet).toSeq.sortBy(delomByKey)

      if (sharedKeys.isEmpty) {
        Nil
      } else {
        val origIndices = sharedKeys.map(origByKey)
        val lisIndices  = longestIncreasingSubsequenceIndices(origIndices)
        lisIndices.map(index => (origByKey(sharedKeys(index)), delomByKey(sharedKeys(index)))).toList
      }
    }

    /** Indices into `values` that form a longest strictly increasing subsequence. */
    private def longestIncreasingSubsequenceIndices(values: Seq[Int]): List[Int] = {
      val predecessors = Array.fill(values.length)(-1)
      val tails        = Array.fill(values.length)(-1)
      var length       = 0

      values.zipWithIndex.foreach { case (value, valueIndex) =>
        var left  = 0
        var right = length
        while (left < right) {
          val mid = (left + right) / 2
          if (values(tails(mid)) < value) left = mid + 1
          else right = mid
        }
        predecessors(valueIndex) = if (left > 0) tails(left - 1) else -1
        tails(left) = valueIndex
        if (left == length) length += 1
      }

      if (length == 0) {
        Nil
      } else {
        var index  = tails(length - 1)
        val result = List.newBuilder[Int]
        while (index >= 0) {
          result += index
          index = predecessors(index)
        }
        result.result().reverse
      }
    }
  }
}
