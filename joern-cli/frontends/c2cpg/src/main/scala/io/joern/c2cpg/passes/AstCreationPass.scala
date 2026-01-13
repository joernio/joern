package io.joern.c2cpg.passes

import io.joern.c2cpg.{C2Cpg, Config}
import io.joern.c2cpg.astcreation.{AstCreator, CGlobal}
import io.joern.c2cpg.parser.{CdtParser, FileDefaults, HeaderFileFinder, JSONCompilationDatabaseParser}
import io.joern.c2cpg.parser.JSONCompilationDatabaseParser.CompilationDatabase
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.utils.{Report, TimeUtils}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass
import org.apache.commons.lang3.StringUtils
import org.eclipse.cdt.core.model.ILanguage
import org.slf4j.{Logger, LoggerFactory}

import java.nio.file.{Path, Paths}

class AstCreationPass(
  cpg: Cpg,
  preprocessedFiles: List[String],
  fileExtensions: Set[String],
  config: Config,
  global: CGlobal,
  report: Report = new Report()
) extends ForkJoinParallelCpgPass[(Path, ILanguage)](cpg) {

  private val compilationDatabase: Option[CompilationDatabase] =
    config.compilationDatabaseFilename.flatMap(JSONCompilationDatabaseParser.parse)

  private val logger: Logger = LoggerFactory.getLogger(classOf[AstCreationPass])

  private val headerFileFinder: HeaderFileFinder = new HeaderFileFinder(config)
  private val parser: CdtParser                  = new CdtParser(config, headerFileFinder, compilationDatabase, global)

  override def generateParts(): Array[(Path, ILanguage)] = {
    val sourceFiles = if (config.compilationDatabaseFilename.isEmpty) {
      sourceFilesFromDirectory()
    } else {
      sourceFilesFromCompilationDatabase(config.compilationDatabaseFilename.get)
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
    compilationDatabase match {
      case Some(db) =>
        val files         = db.commands.map(_.compiledFile()).toList
        val filteredFiles = files.filter(f => fileExtensions.exists(StringUtils.endsWithIgnoreCase(f, _)))
        SourceFiles
          .filterFiles(
            filteredFiles,
            config.inputPath,
            ignoredDefaultRegex = Option(C2Cpg.DefaultIgnoredFolders),
            ignoredFilesRegex = Option(config.ignoredFilesRegex),
            ignoredFilesPath = Option(config.ignoredFiles)
          )
          .toArray
      case None =>
        logger.warn(s"'$compilationDatabaseFile' contains no source files. CPG will be empty.")
        Array.empty
    }
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
          try {
            val localDiff = new AstCreator(relPath, global, config, translationUnit, headerFileFinder).createAst()
            diffGraph.absorb(localDiff)
            logger.debug(s"Generated a CPG for: '$relPath'")
            true
          } catch {
            case s: StackOverflowError =>
              /**   - Eclipse CDT’s C/C++ type inference/analysis is heavily recursive. For certain real-world inputs
                *     this can create extremely deep call stacks and trigger a JVM `StackOverflowError`.
                *   - `StackOverflowError` is a `VirtualMachineError`. Without an explicit catch, failing type analysis
                *     in a single problematic file would crash the whole process.
                */
              logger.warn(s"Failed to generate a CPG for: '$relPath' (error during type analysis)", s)
              false
            case other: Throwable =>
              logger.warn(s"Failed to generate a CPG for: '$relPath'", other)
              false
          }
        case None =>
          report.addReportInfo(relPath, -1)
          false
      }
    }
    report.updateReport(relPath, gotCpg, duration)
  }

}
