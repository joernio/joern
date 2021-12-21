package io.joern.c2cpg.utils

import org.eclipse.cdt.core.parser.FileContent

import java.nio.charset.CodingErrorAction
import java.nio.file.Path
import scala.io.Codec
import scala.io.Source
import scala.util.Using

object IOUtils {

  def readLinesInFile(filePathAsString: String): Seq[String] =
    Using.resource(Source.fromFile(filePathAsString)(Codec.UTF8.decoder.onMalformedInput(CodingErrorAction.REPLACE))) {
      reader =>
        reader.getLines().toSeq
    }

  def readLineLengthsInFile(filePathAsString: String): Seq[Int] =
    readLinesInFile(filePathAsString).map(_.length + System.lineSeparator().toCharArray.length - 1)

  def readFileAsFileContent(file: Path): FileContent = {
    val lines = readLinesInFile(file.toString)
      .flatMap(_.toCharArray.appendedAll(System.lineSeparator().toCharArray))
      .toArray
    FileContent.create(file.toString, true, lines)
  }

}
