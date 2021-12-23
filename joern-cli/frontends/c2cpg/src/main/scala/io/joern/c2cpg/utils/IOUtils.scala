package io.joern.c2cpg.utils

import org.eclipse.cdt.core.parser.FileContent

import java.io.Reader
import java.math.BigInteger
import java.nio.charset.{CharsetDecoder, CodingErrorAction}
import java.nio.file.Path
import scala.io.{BufferedSource, Codec, Source}
import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.util.Using

object IOUtils {

  /**
    * Creates a new UTF-8 decoder.
    * Sadly, instances of CharsetDecoder are not thread-safe as the doc states:
    * 'Instances of this class are not safe for use by multiple concurrent threads.'
    * (copied from: [[java.nio.charset.CharsetDecoder]])
    *
    * As we are using it in a [[io.shiftleft.passes.ParallelCpgPass]] it needs to be thread-safe.
    * Hence, we make sure to create a new instance everytime.
    */
  private def createDecoder(): CharsetDecoder =
    Codec.UTF8.decoder
      .onMalformedInput(CodingErrorAction.REPLACE)
      .onUnmappableCharacter(CodingErrorAction.REPLACE)

  private val validUnicodeRegex = """([a-zA-Z0-9]){4}""".r

  private val boms = Set(
    '\uefbb', // UTF-8
    '\ufeff', // UTF-16 (BE)
    '\ufffe' // UTF-16 (LE)
  )

  private def skipBOMIfPresent(reader: Reader): Unit = {
    reader.mark(1)
    val possibleBOM = new Array[Char](1)
    reader.read(possibleBOM)
    if (!boms.contains(possibleBOM(0))) {
      reader.reset()
    }
  }

  private def removeUnpairedSurrogates(input: String): String = {
    var result = input
    """(\\u)""".r.findAllMatchIn(input).foreach { pos =>
      val matchedString = input.substring(pos.start + 2, pos.start + 6)
      if (validUnicodeRegex.matches(matchedString)) {
        val c = new BigInteger(matchedString, 16).intValue().asInstanceOf[Char]
        if (Character.isLowSurrogate(c) || Character.isHighSurrogate(c)) {
          // removing them including leading '\' (needs escapes for backslash itself + for the regex construction)
          result = result.replaceAll("(\\\\)*\\\\u" + matchedString, "")
        }
      }
    }
    result
  }

  private def contentFromBufferedSource(bufferedSource: BufferedSource): Seq[String] = {
    val reader = bufferedSource.bufferedReader()
    skipBOMIfPresent(reader)
    reader.lines().iterator().asScala.map(removeUnpairedSurrogates).toSeq
  }

  private def bufferedSourceFromFile(path: Path): BufferedSource = {
    Source.fromFile(path.toFile)(createDecoder())
  }

  def readLinesInFile(path: Path): Seq[String] =
    Using.resource(bufferedSourceFromFile(path)) { bufferedSource =>
      contentFromBufferedSource(bufferedSource)
    }

  def readLineLengthsInFile(path: Path): Seq[Int] =
    readLinesInFile(path).map(_.length + System.lineSeparator().toCharArray.length - 1)

  def readFileAsFileContent(path: Path): FileContent = {
    val lines = readLinesInFile(path)
      .flatMap(_.toCharArray.appendedAll(System.lineSeparator().toCharArray))
      .toArray
    FileContent.create(path.toString, true, lines)
  }

}
