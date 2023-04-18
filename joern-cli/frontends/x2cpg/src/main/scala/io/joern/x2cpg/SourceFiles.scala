package io.joern.x2cpg

import better.files.File.VisitOptions
import better.files._

import java.nio.file.Paths
import scala.util.Using

object SourceFiles {

  /** For a given input path, determine all source files by inspecting filename extensions.
    */
  def determine(inputPath: String, sourceFileExtensions: Set[String]): List[String] = {
    determine(Set(inputPath), sourceFileExtensions)
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

  /** For the given file at `filePath` determine the line separator used.
    *
    * Note: the current systems line separator (returned by System.getProperty("line.separator")) can not be used as the
    * file could have been written by another app on another operating system.
    *
    * We only read until the first occurrence of a line break as that is sufficient and should not hinder performance
    * too much.
    */
  def retrieveLineSeparator(filePath: String): String = {
    val file          = new java.io.File(filePath)
    var currentChar   = '\u0000'
    var lineSeparator = ""
    Using(new java.io.FileInputStream(file)) { inputStream =>
      while ({ inputStream.available > 0 && lineSeparator.isEmpty }) {
        currentChar = inputStream.read.asInstanceOf[Char]
        if ((currentChar == '\n') || (currentChar == '\r')) {
          lineSeparator += currentChar
          if (inputStream.available > 0) {
            val next = inputStream.read.asInstanceOf[Char]
            if ((next != currentChar) && ((next == '\r') || (next == '\n'))) {
              lineSeparator += next
            }
          }
        }
      }
      if (lineSeparator.isEmpty) "\n" else lineSeparator
    }.getOrElse("\n")
  }

  /** Constructs an absolute path against rootPath. If the given path is already absolute this path is returned
    * unaltered. Otherwise, "rootPath / path" is returned.
    */
  def toAbsolutePath(path: String, rootPath: String): String = {
    val absolutePath = Paths.get(path) match {
      case p if p.isAbsolute => p
      case f                 => Paths.get(rootPath, f.toString)
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
      projectPath.relativize(absolutePath).toString
    } else {
      path
    }
  }

}
