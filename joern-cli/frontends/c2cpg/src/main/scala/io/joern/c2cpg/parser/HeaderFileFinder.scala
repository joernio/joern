package io.joern.c2cpg.parser

import better.files.*
import io.joern.c2cpg.C2Cpg.DefaultIgnoredFolders
import io.joern.c2cpg.Config
import io.joern.x2cpg.SourceFiles
import org.jline.utils.Levenshtein

class HeaderFileFinder(config: Config) {

  private val nameToPathMap: Map[String, List[String]] = SourceFiles
    .determine(
      config.inputPath,
      FileDefaults.HeaderFileExtensions,
      ignoredDefaultRegex = Option(DefaultIgnoredFolders),
      ignoredFilesRegex = Option(config.ignoredFilesRegex),
      ignoredFilesPath = Option(config.ignoredFiles)
    )
    .map { p =>
      val file = File(p)
      (file.name, file.pathAsString)
    }
    .groupBy(_._1)
    .map(x => (x._1, x._2.map(_._2)))

  /** Given an unresolved header file, given as a non-existing absolute path, determine whether a header file with the
    * same name can be found anywhere in the code base.
    */
  def find(path: String): Option[String] = File(path).nameOption.flatMap { name =>
    val matches = nameToPathMap.getOrElse(name, List())
    matches.sortBy(x => Levenshtein.distance(x, path)).headOption
  }

}
