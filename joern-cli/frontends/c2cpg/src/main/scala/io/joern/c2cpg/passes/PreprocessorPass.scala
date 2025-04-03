package io.joern.c2cpg.passes

import io.joern.c2cpg.C2Cpg.DefaultIgnoredFolders
import io.joern.c2cpg.Config
import io.joern.c2cpg.astcreation.CGlobal
import io.joern.c2cpg.parser.{CdtParser, FileDefaults}
import io.joern.c2cpg.parser.HeaderFileFinder
import io.joern.c2cpg.parser.JSONCompilationDatabaseParser
import io.joern.c2cpg.parser.JSONCompilationDatabaseParser.CommandObject
import io.joern.x2cpg.SourceFiles
import org.eclipse.cdt.core.dom.ast.{
  IASTPreprocessorIfdefStatement,
  IASTPreprocessorIfStatement,
  IASTPreprocessorStatement
}
import org.eclipse.cdt.core.model.ILanguage
import org.slf4j.LoggerFactory

import java.nio.file.Path
import java.nio.file.Paths
import scala.collection.mutable

class PreprocessorPass(config: Config) {

  private val logger = LoggerFactory.getLogger(classOf[PreprocessorPass])

  private val compilationDatabase: mutable.LinkedHashSet[CommandObject] =
    config.compilationDatabase.map(JSONCompilationDatabaseParser.parse).getOrElse(mutable.LinkedHashSet.empty)

  private val headerFileFinder = new HeaderFileFinder(config)
  private val global: CGlobal  = new CGlobal()
  private val parser           = new CdtParser(config, headerFileFinder, compilationDatabase, global)

  def run(): Iterable[String] = {
    val sourceFiles = if (config.compilationDatabase.isEmpty) {
      sourceFilesFromDirectory()
    } else {
      sourceFilesFromCompilationDatabase(config.compilationDatabase.get)
    }
    sourceFiles
      .flatMap { file =>
        val path = Paths.get(file).toAbsolutePath
        CdtParser.languageMappingForSourceFile(path, global, config)
      }
      .flatMap(runOnPart)
  }

  private def sourceFilesFromDirectory(): Iterable[String] = {
    SourceFiles
      .determine(
        config.inputPath,
        FileDefaults.SourceFileExtensions,
        ignoredDefaultRegex = Option(DefaultIgnoredFolders),
        ignoredFilesRegex = Option(config.ignoredFilesRegex),
        ignoredFilesPath = Option(config.ignoredFiles)
      )
  }

  private def sourceFilesFromCompilationDatabase(compilationDatabaseFile: String): Iterable[String] = {
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
  }

  private def preprocessorStatement2String(stmt: IASTPreprocessorStatement): Option[String] = {
    stmt match {
      case s: IASTPreprocessorIfStatement =>
        Option(s"${s.getCondition.mkString}${if (s.taken()) "=true" else ""}")
      case s: IASTPreprocessorIfdefStatement =>
        Option(s"${s.getCondition.mkString}${if (s.taken()) "=true" else ""}")
      case _ => None
    }
  }

  private def runOnPart(fileAndLanguage: (Path, ILanguage)): Iterable[String] = {
    val (path, language) = fileAndLanguage
    parser.preprocessorStatements(path, language).flatMap(preprocessorStatement2String).toSet
  }

}
