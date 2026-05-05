package io.joern.c2cpg.passes

import io.joern.c2cpg.C2Cpg
import io.joern.c2cpg.Config
import io.joern.c2cpg.astcreation.AstCreator
import io.joern.c2cpg.parser.CdtParser
import io.joern.c2cpg.parser.CdtParser.HeaderFileParserLanguage
import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.parser.HeaderFileFinder
import io.joern.c2cpg.parser.JSONCompilationDatabaseParser
import io.joern.c2cpg.parser.JSONCompilationDatabaseParser.CompilationDatabase
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.utils.{Report, TimeUtils}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPassWithAccumulator
import org.apache.commons.lang3.StringUtils
import org.eclipse.cdt.core.dom.ast.gnu.cpp.GPPLanguage
import org.eclipse.cdt.core.model.ILanguage
import org.slf4j.{Logger, LoggerFactory}

import java.nio.file.{Path, Paths}
import scala.collection.mutable

object AstCreationPass {

  case class Accumulator(
    usedTypes: mutable.HashSet[String] = mutable.HashSet.empty,
    methodDeclarations: mutable.HashMap[String, FunctionDeclNodePass.MethodInfo] = mutable.HashMap.empty,
    methodDefinitions: mutable.HashSet[String] = mutable.HashSet.empty,
    headerIncludes: mutable.HashMap[String, HeaderFileParserLanguage] = mutable.HashMap.empty
  ) {
    def registerType(typeName: String): Unit = usedTypes.add(typeName)

    def registerMethodDeclaration(fullName: String, methodInfo: FunctionDeclNodePass.MethodInfo): Unit = {
      methodDeclarations.getOrElseUpdate(fullName, methodInfo)
    }

    def registerMethodDefinition(fullName: String): Unit = methodDefinitions.add(fullName)

    /** Updates the parser language information for a header file.
      *
      * This method tracks which language parser (C, C++, or both) should be used for each header file based on where
      * the header is included from. The logic follows:
      *   - If first time seen: Use C parser for headers included from C files, C++ parser for others
      *   - If previously seen in C files only: Keep C parser or upgrade to both parsers if included from C++
      *   - If previously seen in C++ files only: Keep C++ parser or upgrade to both parsers if included from C
      *   - If already seen in both C and C++ files: Keep using both parsers
      *
      * @param foundPath
      *   The full path to the header file being processed
      * @param language
      *   The language to parse it with
      */
    def updateHeaderFileParserLanguage(foundPath: String, language: HeaderFileParserLanguage): Unit = {
      headerIncludes.updateWith(foundPath) {
        case None                           => Some(language)
        case Some(prev) if prev != language => Some(HeaderFileParserLanguage.Both)
        case some                           => some
      }
    }

    def mergeWith(other: Accumulator): Unit = {
      usedTypes ++= other.usedTypes
      other.methodDeclarations.foreach { case (k, v) => methodDeclarations.getOrElseUpdate(k, v) }
      methodDefinitions ++= other.methodDefinitions
      other.headerIncludes.foreach { case (path, lang) => updateHeaderFileParserLanguage(path, lang) }
    }
  }

}

class AstCreationPass(
  cpg: Cpg,
  preprocessedFiles: List[String],
  fileExtensions: Set[String],
  config: Config,
  previousAccumulator: Option[AstCreationPass.Accumulator] = None,
  report: Report = new Report()
) extends ForkJoinParallelCpgPassWithAccumulator[(Path, ILanguage), AstCreationPass.Accumulator](cpg) {

  import AstCreationPass.Accumulator

  private val compilationDatabase: Option[CompilationDatabase] =
    config.compilationDatabaseFilename.flatMap(JSONCompilationDatabaseParser.parse)

  private val logger: Logger = LoggerFactory.getLogger(classOf[AstCreationPass])

  private val headerFileFinder: HeaderFileFinder = new HeaderFileFinder(config)
  private val parser: CdtParser                  = new CdtParser(config, headerFileFinder, compilationDatabase)

  private var _finalAccumulator: Accumulator = Accumulator()

  def accumulatedState(): Accumulator = _finalAccumulator

  def typesSeen(): Set[String] = _finalAccumulator.usedTypes.toSet

  def unhandledMethodDeclarations(): Map[String, FunctionDeclNodePass.MethodInfo] =
    _finalAccumulator.methodDeclarations.filterNot { case (k, _) =>
      _finalAccumulator.methodDefinitions.contains(k)
    }.toMap

  def headerIncludes(): Map[String, HeaderFileParserLanguage] = _finalAccumulator.headerIncludes.toMap

  override def createAccumulator(): Accumulator = Accumulator()

  override def mergeAccumulator(left: Accumulator, right: Accumulator): Unit = left.mergeWith(right)

  override def onAccumulatorComplete(builder: DiffGraphBuilder, accumulator: Accumulator): Unit = {
    previousAccumulator.foreach(accumulator.mergeWith)
    _finalAccumulator = accumulator
  }

  override def generateParts(): Array[(Path, ILanguage)] = {
    val sourceFiles = if (config.compilationDatabaseFilename.isEmpty) {
      sourceFilesFromDirectory()
    } else {
      sourceFilesFromCompilationDatabase(config.compilationDatabaseFilename.get)
    }
    sourceFiles.flatMap { file =>
      val path = Paths.get(file).toAbsolutePath
      CdtParser.languageMappingForSourceFile(path, previousAccumulator.fold(Map.empty)(_.headerIncludes.toMap), config)
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

  private def suffixFromLanguage(relPath: String, language: ILanguage): String = {
    if (FileDefaults.hasCHeaderFileExtension(relPath) && language.isInstanceOf[GPPLanguage]) { "<cpp>" }
    else ""
  }

  override def runOnPart(
    diffGraph: DiffGraphBuilder,
    fileAndLanguage: (Path, ILanguage),
    accumulator: Accumulator
  ): Unit = {
    val (path, language) = fileAndLanguage
    val relPath          = SourceFiles.toRelativePath(path.toString, config.inputPath)
    val (gotCpg, duration) = TimeUtils.time {
      val parseResult = parser.parse(path, language, accumulator)
      parseResult match {
        case Some(translationUnit) =>
          val fileLOC = translationUnit.getRawSignature.linesIterator.size
          report.addReportInfo(relPath, fileLOC, parsed = true)
          try {
            val languageSuffix = suffixFromLanguage(relPath, language)
            val localDiff =
              new AstCreator(relPath, accumulator, config, translationUnit, headerFileFinder, languageSuffix)
                .createAst()
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
