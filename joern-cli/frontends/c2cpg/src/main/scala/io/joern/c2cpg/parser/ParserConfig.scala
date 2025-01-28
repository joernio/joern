package io.joern.c2cpg.parser

import io.joern.c2cpg.Config
import io.joern.c2cpg.parser.JSONCompilationDatabaseParser.CommandObject
import io.joern.c2cpg.utils.IncludeAutoDiscovery
import io.joern.x2cpg.SourceFiles

import java.nio.file.{Path, Paths}
import scala.collection.mutable

object ParserConfig {

  def empty: ParserConfig =
    ParserConfig(
      mutable.LinkedHashSet.empty,
      mutable.LinkedHashSet.empty,
      mutable.LinkedHashSet.empty,
      Map.empty,
      Map.empty,
      Map.empty,
      logProblems = false,
      logPreprocessor = false
    )

  def fromConfig(config: Config, compilationDatabase: mutable.LinkedHashSet[CommandObject]): ParserConfig = {
    val compilationDatabaseDefines  = compilationDatabase.map { c => c.compiledFile() -> c.defines().toMap }.toMap
    val compilationDatabaseIncludes = compilationDatabase.map { c => c.compiledFile() -> c.includes() }.toMap
    val includePaths = config.includePaths.map(p => Paths.get(SourceFiles.toAbsolutePath(p, config.inputPath)))
    val defines = config.defines.map {
      case define if define.contains("=") =>
        val split = define.split("=")
        split.head -> split(1)
      case define => define -> ""
    }.toMap
    ParserConfig(
      mutable.LinkedHashSet.from(includePaths),
      IncludeAutoDiscovery.discoverIncludePathsC(config),
      IncludeAutoDiscovery.discoverIncludePathsCPP(config),
      defines ++ DefaultDefines.DEFAULT_CALL_CONVENTIONS,
      compilationDatabaseDefines,
      compilationDatabaseIncludes,
      config.logProblems,
      config.logPreprocessor
    )
  }

}

case class ParserConfig(
  userIncludePaths: mutable.LinkedHashSet[Path],
  systemIncludePathsC: mutable.LinkedHashSet[Path],
  systemIncludePathsCPP: mutable.LinkedHashSet[Path],
  definedSymbols: Map[String, String],
  definedSymbolsPerFile: Map[String, Map[String, String]],
  includesPerFile: Map[String, mutable.LinkedHashSet[String]],
  logProblems: Boolean,
  logPreprocessor: Boolean
)
