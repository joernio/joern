package io.joern.c2cpg.parser

import io.joern.c2cpg.Config
import io.joern.c2cpg.astcreation.CGlobal
import io.joern.c2cpg.parser.JSONCompilationDatabaseParser.CommandObject
import io.joern.x2cpg.SourceFiles
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.utils.IOUtils
import org.eclipse.cdt.core.dom.ast.IASTPreprocessorStatement
import org.eclipse.cdt.core.dom.ast.IASTTranslationUnit
import org.eclipse.cdt.core.dom.ast.gnu.c.GCCLanguage
import org.eclipse.cdt.core.dom.ast.gnu.cpp.GPPLanguage
import org.eclipse.cdt.core.model.ILanguage
import org.eclipse.cdt.core.parser.DefaultLogService
import org.eclipse.cdt.core.parser.FileContent
import org.eclipse.cdt.core.parser.ScannerInfo
import org.slf4j.LoggerFactory

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.Failure
import scala.util.Success
import scala.util.Try

object CdtParser {

  private val logger = LoggerFactory.getLogger(classOf[CdtParser])

  enum HeaderFileParserLanguage {
    case Unknown, C, Cpp, Both
  }

  private def readFileAsFileContent(file: Path, lines: Option[Array[Char]] = None): FileContent = {
    val codeLines = lines.getOrElse(IOUtils.readLinesInFile(file).mkString("\n").toArray)
    FileContent.create(file.toString, true, codeLines)
  }

  private case class ParseResult(
    translationUnit: Option[IASTTranslationUnit],
    relativeFilePath: Option[String] = None,
    failure: Option[Throwable] = None
  )

  /** Determines the appropriate language parser(s) to use for a given source file.
    *
    * This method examines the file path, looks at its extension, and uses information from the global object to decide
    * which CDT language parser(s) should be used.
    *
    * @param path
    *   The path to the source file to be parsed
    * @param global
    *   The global object containing information about header file includes
    * @param config
    *   The configuration settings
    * @return
    *   A sequence of (Path, ILanguage) pairs. For some header files, this may return both C and C++ parsers. Returns
    *   empty sequence for non-existent files.
    */
  def languageMappingForSourceFile(path: Path, global: CGlobal, config: Config): Seq[(Path, ILanguage)] = {
    path match {
      case p if !Files.isRegularFile(p) =>
        logger.warn(s"File '${p.toString}' does not exist. Check for broken symlinks!")
        Seq.empty
      case p if !FileDefaults.hasCHeaderFileExtension(p.toString) =>
        Seq((p, CdtParser.createParseLanguage(p, IOUtils.readLinesInFile(p).mkString("\n"), config)))
      case p if !global.headerIncludes.containsKey(p.toString) =>
        Seq((p, GCCLanguage.getDefault))
      case p if global.headerIncludes.get(p.toString) == HeaderFileParserLanguage.C =>
        Seq((p, GCCLanguage.getDefault))
      case p if global.headerIncludes.get(p.toString) == HeaderFileParserLanguage.Cpp =>
        Seq((p, GPPLanguage.getDefault))
      case p =>
        Seq((p, GCCLanguage.getDefault), (p, GPPLanguage.getDefault))
    }
  }

  private def createParseLanguage(file: Path, code: String, config: Config): ILanguage = {
    if (FileDefaults.hasCppFileExtension(file.toString) || preprocessedFileIsFromCPPFile(file, code, config)) {
      GPPLanguage.getDefault
    } else {
      GCCLanguage.getDefault
    }
  }

  private def preprocessedFileIsFromCPPFile(file: Path, code: String, config: Config): Boolean = {
    if (config.withPreprocessedFiles && FileDefaults.hasPreprocessedFileExtension(file.toString)) {
      val fileWithoutExt  = file.toString.substring(0, file.toString.lastIndexOf("."))
      val filesWithCPPExt = FileDefaults.CppFileExtensions.map(ext => Paths.get(s"$fileWithoutExt$ext").fileName)
      code.linesIterator.exists(line => filesWithCPPExt.exists(f => line.contains(s"\"$f\"")))
    } else {
      false
    }
  }

}

class CdtParser(
  config: Config,
  headerFileFinder: HeaderFileFinder,
  compilationDatabase: mutable.LinkedHashSet[CommandObject],
  global: CGlobal
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

  def preprocessorStatements(file: Path, language: ILanguage): Iterable[IASTPreprocessorStatement] = {
    parse(file, language).map(t => preprocessorStatements(t)).getOrElse(Iterable.empty)
  }

  def parse(file: Path, language: ILanguage): Option[IASTTranslationUnit] = {
    parseInternal(file, language) match {
      case ParseResult(translationUnit @ Some(_), Some(relativeFilePath), _) =>
        logger.info(s"Parsed '$relativeFilePath'")
        translationUnit
      case ParseResult(_, maybeRelativePath, maybeThrowable) =>
        logger.warn(
          s"Failed to parse '${maybeRelativePath.getOrElse(file.toString)}': ${maybeThrowable.map(extractParseException).getOrElse("Unknown parse error!")}"
        )
        None
    }
  }

  private def parseInternal(file: Path, language: ILanguage): ParseResult = {
    try {
      val relativeFilePath    = SourceFiles.toRelativePath(file.toString, config.inputPath)
      val fileContentProvider = new CustomFileContentProvider(headerFileFinder, file.toString, global)
      val scInfo              = createScannerInfo(file)
      val fContent            = readFileAsFileContent(file)
      val translationUnit     = language.getASTTranslationUnit(fContent, scInfo, fileContentProvider, null, opts, log)
      if (parserConfig.logProblems) logProblems(translationUnit)
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
        Option(translationUnit)
    }
  }

  private def parseInternal(code: String, inFile: Path): IASTTranslationUnit = {
    val fileContent         = FileContent.create(inFile.toString, true, code.toCharArray)
    val fileContentProvider = new CustomFileContentProvider(headerFileFinder, inFile.toString, global)
    val lang                = CdtParser.createParseLanguage(inFile, code, config)
    val scannerInfo         = createScannerInfo(inFile)
    val translationUnit     = lang.getASTTranslationUnit(fileContent, scannerInfo, fileContentProvider, null, opts, log)
    if (parserConfig.logProblems) logProblems(translationUnit)
    if (parserConfig.logPreprocessor) logPreprocessorStatements(translationUnit)
    translationUnit
  }

}
