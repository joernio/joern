package io.joern.c2cpg.parser

import io.joern.c2cpg.Config
import io.joern.c2cpg.parser.JSONCompilationDatabaseParser.CommandObject
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.joern.x2cpg.SourceFiles
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.utils.IOUtils
import org.eclipse.cdt.core.dom.ast.IASTPreprocessorStatement
import org.eclipse.cdt.core.dom.ast.IASTTranslationUnit
import org.eclipse.cdt.core.dom.ast.gnu.c.GCCLanguage
import org.eclipse.cdt.core.dom.ast.gnu.cpp.GPPLanguage
import org.eclipse.cdt.core.model.ILanguage
import org.eclipse.cdt.core.parser.DefaultLogService
import org.eclipse.cdt.core.parser.FileContent
import org.eclipse.cdt.core.parser.ScannerInfo
import org.eclipse.cdt.internal.core.dom.parser.cpp.semantics.CPPVisitor
import org.slf4j.LoggerFactory

import java.nio.file.{Files, NoSuchFileException, Path, Paths}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.Failure
import scala.util.Success
import scala.util.Try

object CdtParser {

  private val logger = LoggerFactory.getLogger(classOf[CdtParser])

  private def readFileAsFileContent(file: Path, lines: Option[Array[Char]] = None): FileContent = {
    val codeLines = lines.getOrElse(IOUtils.readLinesInFile(file).mkString("\n").toArray)
    FileContent.create(file.toString, true, codeLines)
  }

  private case class ParseResult(
    translationUnit: Option[IASTTranslationUnit],
    relativeFilePath: Option[String] = None,
    failure: Option[Throwable] = None
  )

}

class CdtParser(
  config: Config,
  headerFileFinder: HeaderFileFinder,
  compilationDatabase: mutable.LinkedHashSet[CommandObject]
) extends ParseProblemsLogger
    with PreprocessorStatementsLogger {

  import io.joern.c2cpg.parser.CdtParser.*

  private val parserConfig   = ParserConfig.fromConfig(config, compilationDatabase)
  private val definedSymbols = parserConfig.definedSymbols
  private val includePaths   = parserConfig.userIncludePaths
  private val log            = new DefaultLogService

  private var opts: Int =
    ILanguage.OPTION_NO_IMAGE_LOCATIONS | // performance optimization, allows the parser not to create image-locations
      ILanguage.OPTION_SKIP_TRIVIAL_EXPRESSIONS_IN_AGGREGATE_INITIALIZERS // performance optimization, skips trivial expressions in aggregate initializers
  // instructs the parser to skip function and method bodies
  if (config.skipFunctionBodies) opts |= ILanguage.OPTION_SKIP_FUNCTION_BODIES
  // enables parsing of code behind disabled preprocessor defines
  if (config.compilationDatabase.isEmpty && config.defines.isEmpty) opts |= ILanguage.OPTION_PARSE_INACTIVE_CODE

  def preprocessorStatements(file: Path): Iterable[IASTPreprocessorStatement] = {
    parse(file).map(t => preprocessorStatements(t)).getOrElse(Iterable.empty)
  }

  def parse(file: Path): Option[IASTTranslationUnit] = {
    val parseResult = parseInternal(file)
    parseResult match {
      case ParseResult(Some(t), Some(relativeFilePath), _) =>
        logger.info(s"Parsed '$relativeFilePath'")
        Option(t)
      case ParseResult(_, maybeRelativePath, maybeThrowable) =>
        logger.warn(
          s"Failed to parse '${maybeRelativePath.getOrElse(file.toString)}': ${maybeThrowable.map(extractParseException).getOrElse("Unknown parse error!")}"
        )
        None
    }
  }

  private def parseInternal(file: Path): ParseResult = {
    if (Files.isRegularFile(file)) { // handling potentially broken symlinks
      try {
        val relativeFilePath    = SourceFiles.toRelativePath(file.toString, config.inputPath)
        val fileContent         = readFileAsFileContent(file)
        val fileContentProvider = new CustomFileContentProvider(headerFileFinder)
        val lang                = createParseLanguage(file, fileContent.toString)
        val scannerInfo         = createScannerInfo(file)
        val translationUnit = lang.getASTTranslationUnit(fileContent, scannerInfo, fileContentProvider, null, opts, log)
        val problems        = CPPVisitor.getProblems(translationUnit)
        if (parserConfig.logProblems) logProblems(problems.toList)
        if (parserConfig.logPreprocessor) logPreprocessorStatements(translationUnit)
        ParseResult(Option(translationUnit), Some(relativeFilePath))
      } catch {
        case u: UnsupportedClassVersionError =>
          logger.error("c2cpg requires at least JRE-17 to run. Please check your Java Runtime Environment!", u)
          System.exit(1)
          ParseResult(None, failure = Option(u)) // return value to make the compiler happy
        case e: Throwable =>
          ParseResult(None, failure = Option(e))
      }
    } else {
      ParseResult(
        None,
        failure = Option(new NoSuchFileException(s"File '${file.toString}' does not exist. Check for broken symlinks!"))
      )
    }
  }

  private def createParseLanguage(file: Path, code: String): ILanguage = {
    if (FileDefaults.hasCppFileExtension(file.toString) || preprocessedFileIsFromCPPFile(file, code)) {
      GPPLanguage.getDefault
    } else {
      GCCLanguage.getDefault
    }
  }

  private def preprocessedFileIsFromCPPFile(file: Path, code: String): Boolean = {
    if (config.withPreprocessedFiles && FileDefaults.hasPreprocessedFileExtension(file.toString)) {
      val fileWithoutExt  = file.toString.substring(0, file.toString.lastIndexOf("."))
      val filesWithCPPExt = FileDefaults.CppFileExtensions.map(ext => Paths.get(s"$fileWithoutExt$ext").fileName)
      code.linesIterator.exists(line => filesWithCPPExt.exists(f => line.contains(s"\"$f\"")))
    } else {
      false
    }
  }

  private def createScannerInfo(file: Path): ScannerInfo = {
    val additionalIncludes =
      if (FileDefaults.hasCppFileExtension(file.toString)) parserConfig.systemIncludePathsCPP
      else parserConfig.systemIncludePathsC
    val fileSpecificDefines  = parserConfig.definedSymbolsPerFile.getOrElse(file.toString, Map.empty)
    val fileSpecificIncludes = parserConfig.includesPerFile.getOrElse(file.toString, mutable.LinkedHashSet.empty)
    new ScannerInfo(
      (definedSymbols ++ fileSpecificDefines).asJava,
      fileSpecificIncludes.toArray ++ (includePaths ++ additionalIncludes).map(_.toString).toArray
    )
  }

  def parse(code: String, inFile: Path): Option[IASTTranslationUnit] = {
    Try(parseInternal(code, inFile)) match {
      case Failure(exception) =>
        val relativePath = SourceFiles.toRelativePath(inFile.toString, config.inputPath)
        logger.warn(s"Failed to parse '$code' in file '$relativePath': ${extractParseException(exception)}")
        None
      case Success(translationUnit) =>
        Some(translationUnit)
    }
  }

  private def parseInternal(code: String, inFile: Path): IASTTranslationUnit = {
    val fileContent         = FileContent.create(inFile.toString, true, code.toCharArray)
    val fileContentProvider = new CustomFileContentProvider(headerFileFinder)
    val lang                = createParseLanguage(inFile, code)
    val scannerInfo         = createScannerInfo(inFile)
    val translationUnit     = lang.getASTTranslationUnit(fileContent, scannerInfo, fileContentProvider, null, opts, log)
    val problems            = CPPVisitor.getProblems(translationUnit)
    if (parserConfig.logProblems) logProblems(problems.toList)
    if (parserConfig.logPreprocessor) logPreprocessorStatements(translationUnit)
    translationUnit
  }

}
