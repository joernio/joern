package io.joern.x2cpg

import better.files.File.VisitOptions
import better.files._
import org.slf4j.LoggerFactory

import java.nio.file.Paths

object SourceFiles {

  private val logger = LoggerFactory.getLogger(getClass)

  private def isIgnoredByFileList(filePath: String, config: X2CpgConfig[_]): Boolean = {
    val isInIgnoredFiles = config.ignoredFiles.exists {
      case ignorePath if File(ignorePath).isDirectory => filePath.startsWith(ignorePath)
      case ignorePath                                 => filePath == ignorePath
    }
    if (isInIgnoredFiles) {
      logger.debug(s"'$filePath' ignored (--exclude)")
      true
    } else {
      false
    }
  }

  private def isIgnoredByDefault(filePath: String, config: X2CpgConfig[_]): Boolean = {
    val relPath = toRelativePath(filePath, config.inputPath)
    if (config.defaultIgnoredFilesRegex.exists(_.matches(relPath))) {
      logger.debug(s"'$relPath' ignored by default")
      true
    } else {
      false
    }
  }

  private def isIgnoredByRegex(filePath: String, config: X2CpgConfig[_]): Boolean = {
    val relPath               = toRelativePath(filePath, config.inputPath)
    val isInIgnoredFilesRegex = config.ignoredFilesRegex.matches(relPath)
    if (isInIgnoredFilesRegex) {
      logger.debug(s"'$relPath' ignored (--exclude-regex)")
      true
    } else {
      false
    }
  }

  private def filterFiles(files: List[String], config: X2CpgConfig[_]): List[String] = files.filter {
    case filePath if isIgnoredByDefault(filePath, config)  => false
    case filePath if isIgnoredByFileList(filePath, config) => false
    case filePath if isIgnoredByRegex(filePath, config)    => false
    case _                                                 => true
  }

  /** For a given input path, determine all source files by inspecting filename extensions.
    */
  def determine(inputPath: String, sourceFileExtensions: Set[String]): List[String] = {
    determine(Set(inputPath), sourceFileExtensions)
  }

  /** For a given input path, determine all source files by inspecting filename extensions and filter the result
    * according to the given config (by its ignoredFilesRegex and ignoredFiles).
    */
  def determine(inputPath: String, sourceFileExtensions: Set[String], config: X2CpgConfig[_]): List[String] = {
    filterFiles(determine(Set(inputPath), sourceFileExtensions), config)
  }

  /** For a given array of input paths, determine all source files by inspecting filename extensions.
    */
  def determine(inputPaths: Set[String], sourceFileExtensions: Set[String]): List[String] = {
    def hasSourceFileExtension(file: File): Boolean =
      file.extension.exists(sourceFileExtensions.contains)

    val (dirs, files) = inputPaths
      .map(File(_))
      .partition(_.isDirectory)

    val matchingFiles = files.filter(hasSourceFileExtension).map(_.toString)
    val matchingFilesFromDirs = dirs
      .flatMap(_.listRecursively(VisitOptions.follow))
      .filter(hasSourceFileExtension)
      .map(_.pathAsString)

    (matchingFiles ++ matchingFilesFromDirs).toList.sorted
  }

  /** Constructs an absolute path against rootPath. If the given path is already absolute this path is returned
    * unaltered. Otherwise, "rootPath / path" is returned.
    */
  def toAbsolutePath(path: String, rootPath: String): String = {
    val absolutePath = Paths.get(path) match {
      case p if p.isAbsolute            => p
      case _ if rootPath.endsWith(path) => Paths.get(rootPath)
      case p                            => Paths.get(rootPath, p.toString)
    }
    absolutePath.normalize().toString
  }

  /** Constructs a relative path against rootPath. If the given path is not inside rootPath, path is returned unaltered.
    * Otherwise, the path relative to rootPath is returned.
    */
  def toRelativePath(path: String, rootPath: String): String = {
    if (path.startsWith(rootPath)) {
      val absolutePath = Paths.get(path).toAbsolutePath
      val projectPath  = Paths.get(rootPath).toAbsolutePath
      if (absolutePath.compareTo(projectPath) == 0) {
        absolutePath.getFileName.toString
      } else {
        projectPath.relativize(absolutePath).toString
      }
    } else {
      path
    }
  }

}
