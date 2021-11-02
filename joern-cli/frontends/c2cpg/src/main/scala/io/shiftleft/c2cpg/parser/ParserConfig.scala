package io.shiftleft.c2cpg.parser

import java.nio.file.Path

object ParserConfig {

  def empty: ParserConfig = ParserConfig(Set.empty, Map.empty, logProblems = false, logPreprocessor = false)

}

case class ParserConfig(includePaths: Set[Path],
                        definedSymbols: Map[String, String],
                        logProblems: Boolean,
                        logPreprocessor: Boolean)
