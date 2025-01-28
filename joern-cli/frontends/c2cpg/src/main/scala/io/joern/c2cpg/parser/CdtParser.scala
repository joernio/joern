package io.joern.c2cpg.parser

import better.files.File
import io.joern.c2cpg.Config
import io.joern.c2cpg.parser.JSONCompilationDatabaseParser.CommandObject
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

import java.nio.file.NoSuchFileException
import java.nio.file.Path
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.Failure
import scala.util.Success
import scala.util.Try

object CdtParser {

  private val logger = LoggerFactory.getLogger(classOf[CdtParser])

  private case class ParseResult(
    translationUnit: Option[IASTTranslationUnit],
    preprocessorErrorCount: Int = 0,
    problems: Int = 0,
    failure: Option[Throwable] = None
  )

  private def readFileAsFileContent(file: File, lines: Option[Array[Char]] = None): FileContent = {
    val codeLines = lines.getOrElse(IOUtils.readLinesInFile(file.path).mkString("\n").toArray)
    FileContent.create(file.pathAsString, true, codeLines)
  }

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

  // enables parsing of code behind disabled preprocessor defines:
  private var opts: Int = ILanguage.OPTION_PARSE_INACTIVE_CODE
  // instructs the parser to skip function and method bodies
  if (config.skipFunctionBodies) opts |= ILanguage.OPTION_SKIP_FUNCTION_BODIES
  // performance optimization, allows the parser not to create image-locations
  if (config.noImageLocations) opts |= ILanguage.OPTION_NO_IMAGE_LOCATIONS

  private def preprocessedFileIsFromCPPFile(file: Path, code: String): Boolean = {
    if (config.withPreprocessedFiles && FileDefaults.hasPreprocessedFileExtension(file.toString)) {
      val fileWithoutExt  = file.toString.substring(0, file.toString.lastIndexOf("."))
      val filesWithCPPExt = FileDefaults.CppFileExtensions.map(ext => File(s"$fileWithoutExt$ext").name)
      code.linesIterator.exists(line => filesWithCPPExt.exists(f => line.contains(s"\"$f\"")))
    } else {
      false
    }
  }

  private def createParseLanguage(file: Path, code: String): ILanguage = {
    if (FileDefaults.hasCppFileExtension(file.toString) || preprocessedFileIsFromCPPFile(file, code)) {
      GPPLanguage.getDefault
    } else {
      GCCLanguage.getDefault
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

  private def parseInternal(code: String, inFile: File): IASTTranslationUnit = {
    val fileContent         = FileContent.create(inFile.toString, true, code.toCharArray)
    val fileContentProvider = new CustomFileContentProvider(headerFileFinder)
    val lang                = createParseLanguage(inFile.path, code)
    val scannerInfo         = createScannerInfo(inFile.path)
    val translationUnit     = lang.getASTTranslationUnit(fileContent, scannerInfo, fileContentProvider, null, opts, log)
    val problems            = CPPVisitor.getProblems(translationUnit)
    if (parserConfig.logProblems) logProblems(problems.toList)
    if (parserConfig.logPreprocessor) logPreprocessorStatements(translationUnit)
    translationUnit
  }

  private def parseInternal(file: File): ParseResult = {
    if (file.isRegularFile) { // handling potentially broken symlinks
      try {
        val fileContent         = readFileAsFileContent(file.path)
        val fileContentProvider = new CustomFileContentProvider(headerFileFinder)
        val lang                = createParseLanguage(file.path, fileContent.toString)
        val scannerInfo         = createScannerInfo(file.path)
        val translationUnit = lang.getASTTranslationUnit(fileContent, scannerInfo, fileContentProvider, null, opts, log)
        val problems        = CPPVisitor.getProblems(translationUnit)
        if (parserConfig.logProblems) logProblems(problems.toList)
        if (parserConfig.logPreprocessor) logPreprocessorStatements(translationUnit)
        ParseResult(
          Option(translationUnit),
          preprocessorErrorCount = translationUnit.getPreprocessorProblemsCount,
          problems = problems.length
        )
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
        failure =
          Option(new NoSuchFileException(s"File '${file.pathAsString}' does not exist. Check for broken symlinks!"))
      )
    }
  }

  def preprocessorStatements(file: Path): Iterable[IASTPreprocessorStatement] = {
    parse(file).map(t => preprocessorStatements(t)).getOrElse(Iterable.empty)
  }

  def parse(code: String, inFile: Path): Option[IASTTranslationUnit] = {
    Try(parseInternal(code, inFile)) match {
      case Failure(exception) =>
        logger.warn(s"Failed to parse '$code' in file '$inFile': ${extractParseException(exception)}")
        None
      case Success(translationUnit) =>
        Some(translationUnit)
    }
  }

  def parse(file: Path): Option[IASTTranslationUnit] = {
    val parseResult = parseInternal(file)
    parseResult match {
      case ParseResult(Some(t), c, p, _) =>
        logger.info(s"Parsed '${t.getFilePath}' ($c preprocessor error(s), $p problems)")
        Option(t)
      case ParseResult(_, _, _, maybeThrowable) =>
        logger.warn(
          s"Failed to parse '$file': ${maybeThrowable.map(extractParseException).getOrElse("Unknown parse error!")}"
        )
        None
    }
  }

}
