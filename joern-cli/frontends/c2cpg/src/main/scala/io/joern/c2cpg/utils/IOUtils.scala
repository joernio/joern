package io.joern.c2cpg.utils

import org.eclipse.cdt.core.parser.FileContent

import java.nio.charset.CodingErrorAction
import java.nio.file.Path
import scala.io.Codec
import scala.io.Source
import scala.util.Using

object IOUtils {

  private def createDecoder = Codec.UTF8.decoder.onMalformedInput(CodingErrorAction.REPLACE)

  def readLinesInFile(filePathAsString: String): Seq[String] =
    Using.resource(Source.fromFile(filePathAsString)(createDecoder)) { reader =>
      reader.getLines().toSeq
    }

  def readLineLengthsInFile(filePathAsString: String): Seq[Int] =
    readLinesInFile(filePathAsString).map(_.length + 1)

  def readFileAsFileContent(file: Path): FileContent = {
    Using.resource(Source.fromFile(file.toString)(createDecoder)) { reader =>
      val fileLines =
        reader
          .getLines()
          .flatMap(_.toCharArray.appendedAll(System.lineSeparator().toCharArray))
          .toArray

      FileContent.create(file.toString, true, fileLines)
    }
  }

}
