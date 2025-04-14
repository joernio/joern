package io.joern.c2cpg.parser

import io.joern.c2cpg.Config
import io.joern.c2cpg.parser.JSONCompilationDatabaseParser.CommandObject
import io.joern.c2cpg.parser.JSONCompilationDatabaseParser.CompilationDatabase
import io.joern.c2cpg.utils.IncludeAutoDiscovery

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
      Map.empty
    )

  def fromConfig(config: Config, compilationDatabase: Option[CompilationDatabase]): ParserConfig = {
    val commands = compilationDatabase.map(_.commands).getOrElse(Set.empty)
    val compilationDatabaseDefines = commands.map { c =>
      c.compiledFile() -> c.defines().toMap
    }.toMap
    val includes = commands.map { c =>
      c.compiledFile() -> c.includes()
    }.toMap
    val definedSymbols = config.defines.map { define =>
      if (define.contains("=")) {
        val split = define.split("=")
        split.head -> split(1)
      } else {
        define -> ""
      }
    }.toMap ++ DefaultDefines.DEFAULT_CALL_CONVENTIONS
    ParserConfig(
      mutable.LinkedHashSet.from(config.includePaths.map(Paths.get(_).toAbsolutePath)),
      IncludeAutoDiscovery.discoverIncludePathsC(config),
      IncludeAutoDiscovery.discoverIncludePathsCPP(config),
      definedSymbols,
      compilationDatabaseDefines,
      includes
    )
  }

}

case class ParserConfig(
  userIncludePaths: mutable.LinkedHashSet[Path],
  systemIncludePathsC: mutable.LinkedHashSet[Path],
  systemIncludePathsCPP: mutable.LinkedHashSet[Path],
  definedSymbols: Map[String, String],
  definedSymbolsPerFile: Map[String, Map[String, String]],
  includesPerFile: Map[String, mutable.LinkedHashSet[String]]
)
