package io.joern.c2cpg.utils

import org.eclipse.cdt.core.parser.FileContent

import java.nio.file.Path

object IOUtils {

  def readFileAsFileContent(path: Path): FileContent = {
    val lines = io.shiftleft.utils.IOUtils
      .readLinesInFile(path)
      .mkString("\n")
      .toArray
    FileContent.create(path.toString, true, lines)
  }

}
