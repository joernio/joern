package io.shiftleft.c2cpg.utils

import org.eclipse.cdt.core.parser.FileContent

import java.nio.charset.CodingErrorAction
import java.nio.file.{Path, Paths}
import scala.io.{Codec, Source}
import scala.util.Using

object IOUtils {

  def readLinesInFile(filePathAsString: String): Seq[String] =
    readFileAsFileContent(Paths.get(filePathAsString)).toString.split(System.lineSeparator()).toSeq

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
