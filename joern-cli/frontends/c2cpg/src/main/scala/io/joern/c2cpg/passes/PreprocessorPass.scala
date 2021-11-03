package io.joern.c2cpg.passes

import io.joern.c2cpg.parser.{CdtParser, HeaderFileFinder, ParserConfig}
import org.eclipse.cdt.core.dom.ast.{
  IASTPreprocessorIfStatement,
  IASTPreprocessorIfdefStatement,
  IASTPreprocessorStatement
}

import java.nio.file.Paths
import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable
import scala.collection.parallel.immutable.ParIterable

class PreprocessorPass(filenames: List[String], parseConfig: ParserConfig, headerFileFinder: HeaderFileFinder = null) {

  def run(): ParIterable[String] = filenames.par.flatMap(runOnPart)

  private def preprocessorStatement2String(stmt: IASTPreprocessorStatement): Option[String] = stmt match {
    case s: IASTPreprocessorIfStatement =>
      Some(s.getCondition.mkString + { if (s.taken()) "=true" else "" })
    case s: IASTPreprocessorIfdefStatement =>
      Some(s.getCondition.mkString + { if (s.taken()) "=true" else "" })
    case _ => None
  }

  private def runOnPart(filename: String): Iterable[String] =
    new CdtParser(parseConfig, headerFileFinder)
      .preprocessorStatements(Paths.get(filename))
      .flatMap(preprocessorStatement2String)
      .toSet

}
