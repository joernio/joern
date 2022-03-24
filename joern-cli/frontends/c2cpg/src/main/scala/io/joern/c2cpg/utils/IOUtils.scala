package io.joern.c2cpg.utils

import org.eclipse.cdt.core.parser.FileContent

import java.nio.file.Path

object IOUtils {

  def readLineLengthsInFile(path: Path): Seq[Int] =
    io.shiftleft.utils.IOUtils.readLinesInFile(path).map(_.length + System.lineSeparator().toCharArray.length - 1)

  def readFileAsFileContent(path: Path): FileContent = {
    val lines = io.shiftleft.utils.IOUtils
      .readLinesInFile(path)
      .mkString("\n")
      .toArray
    FileContent.create(path.toString, true, lines)
  }

}
