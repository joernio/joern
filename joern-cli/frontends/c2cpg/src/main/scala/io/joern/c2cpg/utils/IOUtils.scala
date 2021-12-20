package io.joern.c2cpg.utils

import org.eclipse.cdt.core.parser.FileContent

import java.nio.charset.CodingErrorAction
import java.nio.file.Path
import scala.io.Codec
import scala.io.Source
import scala.util.Using

object IOUtils {

  def readLinesInFile(filePathAsString: String): Seq[String] = {
    val decoder = Codec.UTF8.decoder.onMalformedInput(CodingErrorAction.REPLACE)
    Using
      .resource(Source.fromFile(filePathAsString)(decoder)) { reader =>
        reader.getLines().map(_ + "\n").toSeq
      }
  }

  def readFileAsFileContent(file: Path): FileContent = {
    val decoder = Codec.UTF8.decoder.onMalformedInput(CodingErrorAction.REPLACE)
    Using.resource(Source.fromFile(file.toString)(decoder)) { reader =>
      val fileLines =
        reader
          .getLines()
          .flatMap(_.toCharArray.appendedAll(System.lineSeparator().toCharArray))
          .toArray
      FileContent.create(file.toString, true, fileLines)
    }
  }

}
