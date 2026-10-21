package io.joern.javasrc2cpg.util

import java.nio.file.Path
import scala.util.matching.Regex
import scala.collection.mutable.ArrayBuffer

private type RegexSplitResult = (regexMatch: Regex.Match, lineGroup: Seq[String])

/** Delombok
  */
object DelombokStderrFilter {

  private val ErrorHeaderRegex = raw"^((?:/|[A-Za-z]:[\\/])[^:]+):(\d+):\s*error:\s*(.*)$$".r
  private val FileStatusRegex  = raw"^File: (.+) \[(delomboked|unchanged)]$$".r

  def splitWhen(seq: Seq[String])(predicate: String => Boolean): Iterator[Array[String]] = {
    val it = seq.iterator.buffered

    new Iterator[Array[String]] {
      def hasNext: Boolean = it.hasNext

      def next(): Array[String] = {
        if (!hasNext) throw new NoSuchElementException("next on empty iterator")

        val buffer = ArrayBuffer[String]()

        // Unconditionally take the first element to start the group
        buffer += it.next()

        // Keep taking elements until the NEXT element matches the predicate
        while (it.hasNext && !predicate(it.head)) {
          buffer += it.next()
        }

        buffer.toArray
      }
    }
  }

  /** Filter the given stderr lines: drop records that are not contained within the current packageRoot; keep everything
    * else verbatim. Order is preserved.
    */
  def filter(packageRoot: Path, stderrLines: Seq[String]): Seq[String] = {
    splitWhen(stderrLines)(line => ErrorHeaderRegex.matches(line) || FileStatusRegex.matches(line)).flatMap {
      lineGroup =>
        ErrorHeaderRegex.findFirstMatchIn(lineGroup.head) match {
          case None =>
            // If the first line doesn't match the error header, add the line group to the output to avoid silently
            // missing warnings at the start of the logs.
            // Otherwise, if it is a file status line, output it as well
            lineGroup

          case Some(regexMatch) =>
            val filePath = Path.of(regexMatch.group(1)).toAbsolutePath

            if (filePath.startsWith(packageRoot)) {
              lineGroup
            } else
              Nil
        }
    }.toSeq
  }
}
