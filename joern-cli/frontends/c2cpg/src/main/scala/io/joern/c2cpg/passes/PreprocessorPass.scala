package io.joern.c2cpg.passes

import io.joern.c2cpg.C2Cpg.DefaultIgnoredFolders
import io.joern.c2cpg.Config
import io.joern.c2cpg.parser.{CdtParser, FileDefaults}
import io.joern.c2cpg.parser.JSONCompilationDatabaseParser
import io.joern.c2cpg.parser.JSONCompilationDatabaseParser.CommandObject
import io.joern.x2cpg.SourceFiles
import org.eclipse.cdt.core.dom.ast.{
  IASTPreprocessorIfdefStatement,
  IASTPreprocessorIfStatement,
  IASTPreprocessorStatement
}
import org.slf4j.LoggerFactory

import java.nio.file.Paths
import scala.collection.mutable
import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable
import scala.collection.parallel.immutable.ParIterable

class PreprocessorPass(config: Config) {

  private val logger = LoggerFactory.getLogger(classOf[PreprocessorPass])

  private val compilationDatabase: mutable.LinkedHashSet[CommandObject] =
    config.compilationDatabase.map(JSONCompilationDatabaseParser.parse).getOrElse(mutable.LinkedHashSet.empty)

  private val parser = new CdtParser(config, compilationDatabase)

  private def sourceFilesFromDirectory(): ParIterable[String] = {
    SourceFiles
      .determine(
        config.inputPath,
        FileDefaults.SourceFileExtensions,
        ignoredDefaultRegex = Option(DefaultIgnoredFolders),
        ignoredFilesRegex = Option(config.ignoredFilesRegex),
        ignoredFilesPath = Option(config.ignoredFiles)
      )
      .par
      .flatMap(runOnPart)
  }

  private def sourceFilesFromCompilationDatabase(compilationDatabaseFile: String): ParIterable[String] = {
    if (compilationDatabase.isEmpty) {
      logger.warn(s"'$compilationDatabaseFile' contains no source files.")
    }
    SourceFiles
      .filterFiles(
        compilationDatabase.map(_.compiledFile()).toList,
        config.inputPath,
        ignoredDefaultRegex = Option(DefaultIgnoredFolders),
        ignoredFilesRegex = Option(config.ignoredFilesRegex),
        ignoredFilesPath = Option(config.ignoredFiles)
      )
      .par
      .flatMap(runOnPart)
  }

  def run(): ParIterable[String] = {
    if (config.compilationDatabase.isEmpty) {
      sourceFilesFromDirectory()
    } else {
      sourceFilesFromCompilationDatabase(config.compilationDatabase.get)
    }

  }

  private def preprocessorStatement2String(stmt: IASTPreprocessorStatement): Option[String] = stmt match {
    case s: IASTPreprocessorIfStatement =>
      Option(s"${s.getCondition.mkString}${if (s.taken()) "=true" else ""}")
    case s: IASTPreprocessorIfdefStatement =>
      Option(s"${s.getCondition.mkString}${if (s.taken()) "=true" else ""}")
    case _ => None
  }

  private def runOnPart(filename: String): Iterable[String] =
    parser.preprocessorStatements(Paths.get(filename)).flatMap(preprocessorStatement2String).toSet

}
