package io.joern.jssrc2cpg.utils

import java.nio.file.Path

object IOUtils {

  def readLineLengthsInFile(path: Path): Seq[Int] =
    io.shiftleft.utils.IOUtils.readLinesInFile(path).map(_.length + System.lineSeparator().toCharArray.length - 1)

}
