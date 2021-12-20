package io.joern.c2cpg.utils

import org.eclipse.cdt.core.parser.FileContent

import java.nio.charset.CodingErrorAction
import java.nio.file.Path
import scala.io.Codec
import scala.io.Source
import scala.util.Using

object IOUtils {

  def readLinesInFile(filePathAsString: String, sep: String = "\n"): Seq[String] = {
    val decoder = Codec.UTF8.decoder.onMalformedInput(CodingErrorAction.REPLACE)
    Using.resource(Source.fromFile(filePathAsString)(decoder)) { r =>
      r.getLines().map(_ + sep).toSeq
    }
  }

  def readLineLengthsInFile(filePathAsString: String): Seq[Int] =
    readLinesInFile(filePathAsString).map(_.length)

  def readFileAsFileContent(file: Path): FileContent = {
    val lines = readLinesInFile(file.toString, System.lineSeparator()).flatMap(_.toCharArray).toArray
    FileContent.create(file.toString, true, lines)
  }

}
