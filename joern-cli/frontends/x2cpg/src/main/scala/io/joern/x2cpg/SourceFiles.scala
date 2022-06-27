package io.joern.x2cpg

import better.files._

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
      .flatMap(_.listRecursively.filter(hasSourceFileExtension))
      .map(_.toString)

    (matchingFiles ++ matchingFilesFromDirs).toList.sorted
  }
}
