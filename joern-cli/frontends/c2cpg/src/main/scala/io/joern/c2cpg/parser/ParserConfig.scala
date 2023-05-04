package io.joern.c2cpg.parser

import io.joern.c2cpg.Config
import io.joern.c2cpg.utils.IncludeAutoDiscovery

import java.nio.file.{Path, Paths}

object ParserConfig {

  def empty: ParserConfig =
    ParserConfig(Set.empty, Set.empty, Set.empty, Map.empty, logProblems = false, logPreprocessor = false)

  def fromConfig(config: Config): ParserConfig = ParserConfig(
    config.includePaths.map(Paths.get(_).toAbsolutePath),
    IncludeAutoDiscovery.discoverIncludePathsC(config),
    IncludeAutoDiscovery.discoverIncludePathsCPP(config),
    config.defines.map {
      case define if define.contains("=") =>
        val s = define.split("=")
        s.head -> s(1)
      case define => define -> "true"
    }.toMap ++ DefaultDefines.DEFAULT_CALL_CONVENTIONS,
    config.logProblems,
    config.logPreprocessor
  )

}

case class ParserConfig(
  userIncludePaths: Set[Path],
  systemIncludePathsC: Set[Path],
  systemIncludePathsCPP: Set[Path],
  definedSymbols: Map[String, String],
  logProblems: Boolean,
  logPreprocessor: Boolean
)
