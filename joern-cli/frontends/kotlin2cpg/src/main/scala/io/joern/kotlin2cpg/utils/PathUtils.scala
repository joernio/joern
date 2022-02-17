package io.joern.kotlin2cpg.utils

import java.nio.file.Paths

object PathUtils {
  def relativize(sourceDir: String, filename: String): String = {
    val pathAbsolute = Paths.get(filename).toAbsolutePath
    val pathBase     = Paths.get(sourceDir).toAbsolutePath
    pathBase.relativize(pathAbsolute).toString
  }
}
