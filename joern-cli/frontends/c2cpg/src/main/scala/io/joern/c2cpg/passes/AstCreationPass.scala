package io.joern.c2cpg.passes

import io.joern.c2cpg.C2Cpg.DefaultIgnoredFolders
import io.joern.c2cpg.Config
import io.joern.c2cpg.astcreation.AstCreator
import io.joern.c2cpg.astcreation.CGlobal
import io.joern.c2cpg.parser.CdtParser
import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.parser.JSONCompilationDatabaseParser
import io.joern.c2cpg.parser.JSONCompilationDatabaseParser.CommandObject
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.utils.Report
import io.joern.x2cpg.utils.TimeUtils
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass
import org.slf4j.Logger
import org.slf4j.LoggerFactory

import java.nio.file.Paths
import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.util.matching.Regex

class AstCreationPass(cpg: Cpg, config: Config, report: Report = new Report())
    extends ForkJoinParallelCpgPass[String](cpg) {

  private val logger: Logger = LoggerFactory.getLogger(classOf[AstCreationPass])

  private val global                                                  = new CGlobal()
  private val file2OffsetTable: ConcurrentHashMap[String, Array[Int]] = new ConcurrentHashMap()

  private val compilationDatabase: mutable.LinkedHashSet[CommandObject] =
    config.compilationDatabase.map(JSONCompilationDatabaseParser.parse).getOrElse(mutable.LinkedHashSet.empty)

  private val parser: CdtParser = new CdtParser(config, compilationDatabase)

  def typesSeen(): List[String] = global.usedTypes.keys().asScala.toList

  def unhandledMethodDeclarations(): Map[String, CGlobal.MethodInfo] = {
    global.methodDeclarations.asScala.toMap -- global.methodDefinitions.asScala.keys
  }

  private def sourceFilesFromDirectory(): Array[String] = {
    val sourceFileExtensions =
      FileDefaults.SourceFileExtensions ++
        FileDefaults.HeaderFileExtensions ++
        Option.when(config.withPreprocessedFiles)(FileDefaults.PreprocessedExt).toList
    val allSourceFiles = SourceFiles
      .determine(
        config.inputPath,
        sourceFileExtensions,
        ignoredDefaultRegex = Option(DefaultIgnoredFolders),
        ignoredFilesRegex = Option(config.ignoredFilesRegex),
        ignoredFilesPath = Option(config.ignoredFiles)
      )
      .toArray
    if (config.withPreprocessedFiles) {
      allSourceFiles.filter {
        case f if !FileDefaults.hasPreprocessedFileExtension(f) =>
          val fAsPreprocessedFile = s"${f.substring(0, f.lastIndexOf("."))}${FileDefaults.PreprocessedExt}"
          !allSourceFiles.exists { sourceFile => f != sourceFile && sourceFile == fAsPreprocessedFile }
        case _ => true
      }
    } else {
      allSourceFiles
    }
  }

  private def sourceFilesFromCompilationDatabase(compilationDatabaseFile: String): Array[String] = {
    if (compilationDatabase.isEmpty) {
      logger.warn(s"'$compilationDatabaseFile' contains no source files. CPG will be empty.")
    }
    SourceFiles
      .filterFiles(
        compilationDatabase.map(_.compiledFile()).toList,
        config.inputPath,
        ignoredDefaultRegex = Option(DefaultIgnoredFolders),
        ignoredFilesRegex = Option(config.ignoredFilesRegex),
        ignoredFilesPath = Option(config.ignoredFiles)
      )
      .toArray
  }

  override def generateParts(): Array[String] = {
    if (config.compilationDatabase.isEmpty) {
      sourceFilesFromDirectory()
    } else {
      sourceFilesFromCompilationDatabase(config.compilationDatabase.get)
    }
  }

  override def runOnPart(diffGraph: DiffGraphBuilder, filename: String): Unit = {
    val path    = Paths.get(filename).toAbsolutePath
    val relPath = SourceFiles.toRelativePath(path.toString, config.inputPath)
    val fileLOC = io.shiftleft.utils.IOUtils.readLinesInFile(path).size
    val (gotCpg, duration) = TimeUtils.time {
      val parseResult = parser.parse(path)
      parseResult match {
        case Some(translationUnit) =>
          report.addReportInfo(relPath, fileLOC, parsed = true)
          Try {
            val localDiff = new AstCreator(relPath, global, config, translationUnit, file2OffsetTable)(
              config.schemaValidation
            ).createAst()
            diffGraph.absorb(localDiff)
          } match {
            case Failure(exception) =>
              logger.warn(s"Failed to generate a CPG for: '$filename'", exception)
              false
            case Success(_) => true
          }
        case None =>
          report.addReportInfo(relPath, fileLOC)
          false
      }
    }
    report.updateReport(relPath, gotCpg, duration)
  }

}
