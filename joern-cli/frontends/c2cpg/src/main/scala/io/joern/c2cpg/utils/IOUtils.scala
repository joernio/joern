package io.joern.c2cpg.utils

import io.joern.c2cpg.Config
import org.eclipse.cdt.core.parser.FileContent

import java.nio.file.{Path, Paths}

object IOUtils {

  def toAbsolutePath(path: String, config: Config): String = path match {
    // the path may be relative for project files but absolute for system header files
    case f if Paths.get(f).isAbsolute => Paths.get(f).toString
    case f                            => Paths.get(config.inputPath, f).toString
  }

  def toRelativePath(path: String, config: Config): String = {
    if (path.contains(config.inputPath)) {
      val absolutePath = Paths.get(path).toAbsolutePath
      val projectPath  = Paths.get(config.inputPath).toAbsolutePath
      projectPath.relativize(absolutePath).toString
    } else {
      path
    }
  }

  def readFileAsFileContent(path: Path): FileContent = {
    val lines = io.shiftleft.utils.IOUtils
      .readLinesInFile(path)
      .mkString("\n")
      .toArray
    FileContent.create(path.toString, true, lines)
  }

}
