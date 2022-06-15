package io.joern.jssrc2cpg.utils

import java.nio.file.Path

object JsIOUtils {

  def readFile(path: Path): String = {
    val bufferedSource = scala.io.Source.fromFile(path.toFile)
    val result         = bufferedSource.getLines().mkString("", "\n", "\n")
    bufferedSource.close()
    result
  }

  def countLinesInFile(path: Path): Int = {
    val bufferedSource = scala.io.Source.fromFile(path.toFile)
    val result         = bufferedSource.getLines().size
    bufferedSource.close()
    result
  }

}
