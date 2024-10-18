package io.joern.c2cpg.parser

import io.joern.c2cpg.Config
import io.joern.c2cpg.parser.JSONCompilationDatabaseParser.CommandObject
import io.joern.c2cpg.utils.IncludeAutoDiscovery

import java.nio.file.{Path, Paths}

object ParserConfig {

  def empty: ParserConfig =
    ParserConfig(
      Set.empty,
      Set.empty,
      Set.empty,
      Map.empty,
      Map.empty,
      Map.empty,
      logProblems = false,
      logPreprocessor = false
    )

  def fromConfig(config: Config, compilationDatabase: List[CommandObject]): ParserConfig = {
    val compilationDatabaseDefines = compilationDatabase.map { c =>
      c.compiledFile() -> c.defines().toMap
    }.toMap
    val includes = compilationDatabase.map { c =>
      c.compiledFile() -> c.includes()
    }.toMap
    ParserConfig(
      config.includePaths.map(Paths.get(_).toAbsolutePath),
      IncludeAutoDiscovery.discoverIncludePathsC(config),
      IncludeAutoDiscovery.discoverIncludePathsCPP(config),
      config.defines.map { define =>
        if (define.contains("=")) {
          val split = define.split("=")
          split.head -> split(1)
        } else {
          define -> ""
        }
      }.toMap ++ DefaultDefines.DEFAULT_CALL_CONVENTIONS,
      compilationDatabaseDefines,
      includes,
      config.logProblems,
      config.logPreprocessor
    )
  }

}

case class ParserConfig(
  userIncludePaths: Set[Path],
  systemIncludePathsC: Set[Path],
  systemIncludePathsCPP: Set[Path],
  definedSymbols: Map[String, String],
  definedSymbolsPerFile: Map[String, Map[String, String]],
  includesPerFile: Map[String, List[String]],
  logProblems: Boolean,
  logPreprocessor: Boolean
)
