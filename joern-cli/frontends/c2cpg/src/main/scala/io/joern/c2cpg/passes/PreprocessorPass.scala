package io.joern.c2cpg.passes

import io.joern.c2cpg.C2Cpg.Config
import io.joern.c2cpg.parser.{CdtParser, FileDefaults, HeaderFileFinder, ParserConfig}
import io.shiftleft.x2cpg.SourceFiles
import org.eclipse.cdt.core.dom.ast.{
  IASTPreprocessorIfStatement,
  IASTPreprocessorIfdefStatement,
  IASTPreprocessorStatement
}

import java.nio.file.Paths
import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable
import scala.collection.parallel.immutable.ParIterable

class PreprocessorPass(config: Config) {

  def run(): ParIterable[String] =
    SourceFiles.determine(config.inputPaths, FileDefaults.SOURCE_FILE_EXTENSIONS).par.flatMap(runOnPart)

  private def preprocessorStatement2String(stmt: IASTPreprocessorStatement): Option[String] = stmt match {
    case s: IASTPreprocessorIfStatement =>
      Some(s.getCondition.mkString + { if (s.taken()) "=true" else "" })
    case s: IASTPreprocessorIfdefStatement =>
      Some(s.getCondition.mkString + { if (s.taken()) "=true" else "" })
    case _ => None
  }

  private def runOnPart(filename: String): Iterable[String] =
    new CdtParser(ParserConfig.fromConfig(config), new HeaderFileFinder(config.inputPaths))
      .preprocessorStatements(Paths.get(filename))
      .flatMap(preprocessorStatement2String)
      .toSet

}
