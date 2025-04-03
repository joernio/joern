package io.joern.c2cpg.passes

import io.joern.c2cpg.Config
import io.joern.c2cpg.astcreation.AstCreator
import io.joern.c2cpg.astcreation.CGlobal
import io.joern.c2cpg.parser.CdtParser
import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.parser.HeaderFileFinder
import io.joern.c2cpg.parser.JSONCompilationDatabaseParser
import io.joern.c2cpg.parser.JSONCompilationDatabaseParser.CommandObject
import io.joern.c2cpg.C2Cpg
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.utils.Report
import io.joern.x2cpg.utils.TimeUtils
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass
import org.apache.commons.lang3.StringUtils
import org.eclipse.cdt.core.model.ILanguage
import org.slf4j.Logger
import org.slf4j.LoggerFactory

import java.nio.file.Path
import java.nio.file.Paths
import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable
import scala.util.Failure
import scala.util.Success
import scala.util.Try

class AstCreationPass(
  cpg: Cpg,
  preprocessedFiles: List[String],
  fileExtensions: Set[String],
  config: Config,
  global: CGlobal,
  report: Report = new Report()
) extends ForkJoinParallelCpgPass[(Path, ILanguage)](cpg) {

  private val logger: Logger = LoggerFactory.getLogger(classOf[AstCreationPass])

  private val headerFileFinder                                        = new HeaderFileFinder(config)
  private val file2OffsetTable: ConcurrentHashMap[String, Array[Int]] = new ConcurrentHashMap()

  private val compilationDatabase: mutable.LinkedHashSet[CommandObject] =
    config.compilationDatabase.map(JSONCompilationDatabaseParser.parse).getOrElse(mutable.LinkedHashSet.empty)

  private val parser: CdtParser = new CdtParser(config, headerFileFinder, compilationDatabase, global)

  override def generateParts(): Array[(Path, ILanguage)] = {
    val sourceFiles = if (config.compilationDatabase.isEmpty) {
      sourceFilesFromDirectory()
    } else {
      sourceFilesFromCompilationDatabase(config.compilationDatabase.get)
    }
    sourceFiles.flatMap { file =>
      val path = Paths.get(file).toAbsolutePath
      CdtParser.languageMappingForSourceFile(path, global, config)
    }
  }

  private def sourceFilesFromDirectory(): Array[String] = {
    val allSourceFiles = SourceFiles
      .determine(
        config.inputPath,
        fileExtensions,
        ignoredDefaultRegex = Option(C2Cpg.DefaultIgnoredFolders),
        ignoredFilesRegex = Option(config.ignoredFilesRegex),
        ignoredFilesPath = Option(config.ignoredFiles)
      )
      .toArray
    if (config.withPreprocessedFiles) {
      allSourceFiles.filter {
        case f if !FileDefaults.hasPreprocessedFileExtension(f) =>
          val fAsPreprocessedFile = s"${f.substring(0, f.lastIndexOf("."))}${FileDefaults.PreprocessedExt}"
          !preprocessedFiles.exists { sourceFile => f != sourceFile && sourceFile == fAsPreprocessedFile }
        case _ => true
      }
    } else {
      allSourceFiles
    }
  }

  private def sourceFilesFromCompilationDatabase(compilationDatabaseFile: String): Array[String] = {
    if (compilationDatabase.isEmpty) {
      logger.warn(s"'$compilationDatabaseFile' contains no source files. CPG will be empty.")
      return Array.empty
    }
    val allSourceFiles =
      compilationDatabase.map(_.compiledFile()).toList
    val filteredSourceFiles =
      allSourceFiles.filter(file => fileExtensions.exists(StringUtils.endsWithIgnoreCase(file, _)))
    SourceFiles
      .filterFiles(
        filteredSourceFiles,
        config.inputPath,
        ignoredDefaultRegex = Option(C2Cpg.DefaultIgnoredFolders),
        ignoredFilesRegex = Option(config.ignoredFilesRegex),
        ignoredFilesPath = Option(config.ignoredFiles)
      )
      .toArray
  }

  override def runOnPart(diffGraph: DiffGraphBuilder, fileAndLanguage: (Path, ILanguage)): Unit = {
    val (path, language) = fileAndLanguage
    val relPath          = SourceFiles.toRelativePath(path.toString, config.inputPath)
    val (gotCpg, duration) = TimeUtils.time {
      val parseResult = parser.parse(path, language)
      parseResult match {
        case Some(translationUnit) =>
          val fileLOC = translationUnit.getRawSignature.linesIterator.size
          report.addReportInfo(relPath, fileLOC, parsed = true)
          Try {
            val localDiff =
              new AstCreator(relPath, global, config, translationUnit, headerFileFinder, file2OffsetTable).createAst()
            diffGraph.absorb(localDiff)
          } match {
            case Failure(exception) =>
              logger.warn(s"Failed to generate a CPG for: '$relPath'", exception)
              false
            case Success(_) => true
          }
        case None =>
          report.addReportInfo(relPath, -1)
          false
      }
    }
    report.updateReport(relPath, gotCpg, duration)
  }

}
