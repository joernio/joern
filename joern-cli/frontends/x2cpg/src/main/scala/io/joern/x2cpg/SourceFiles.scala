package io.joern.x2cpg

import better.files.File.VisitOptions
import better.files._

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
}
