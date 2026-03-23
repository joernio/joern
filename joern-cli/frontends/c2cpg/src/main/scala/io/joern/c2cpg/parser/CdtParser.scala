package io.joern.c2cpg.parser

import io.joern.c2cpg.Config
import io.joern.c2cpg.astcreation.CGlobal
import io.joern.c2cpg.parser.JSONCompilationDatabaseParser.CompilationDatabase
import io.joern.x2cpg.SourceFiles
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.utils.IOUtils
import org.eclipse.cdt.core.dom.ast.{IASTPreprocessorStatement, IASTTranslationUnit}
import org.eclipse.cdt.core.dom.ast.gnu.c.GCCLanguage
import org.eclipse.cdt.core.dom.ast.gnu.cpp.GPPLanguage
import org.eclipse.cdt.core.model.ILanguage
import org.eclipse.cdt.core.parser.{DefaultLogService, FileContent, ScannerInfo}
import org.slf4j.LoggerFactory

import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

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
    if (!Files.isRegularFile(path)) {
      logger.warn(s"File '${path.toString}' does not exist. Check for broken symlinks!")
      return Seq.empty
    }
    val filePath = path.toString
    if (!FileDefaults.hasCHeaderFileExtension(filePath)) {
      val code = IOUtils.readLinesInFile(path).mkString("\n")
      return Seq((path, createParseLanguage(path, code, config)))
    }
    global.headerIncludes.get(filePath) match {
      case null | HeaderFileParserLanguage.C => Seq((path, GCCLanguage.getDefault))
      case HeaderFileParserLanguage.Cpp      => Seq((path, GPPLanguage.getDefault))
      case _                                 => Seq((path, GCCLanguage.getDefault), (path, GPPLanguage.getDefault))
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
  compilationDatabase: Option[CompilationDatabase],
  global: CGlobal
) extends ParseProblemsLogger
    with PreprocessorStatementsLogger {

  import io.joern.c2cpg.parser.CdtParser.*

  private val parserConfig = ParserConfig.fromConfig(config, compilationDatabase)
  private val log          = new DefaultLogService

  private var opts: Int =
    ILanguage.OPTION_NO_IMAGE_LOCATIONS | // performance optimization, allows the parser not to create image-locations
      ILanguage.OPTION_SKIP_TRIVIAL_EXPRESSIONS_IN_AGGREGATE_INITIALIZERS // performance optimization, skips trivial expressions in aggregate initializers
  // instructs the parser to skip function and method bodies
  if (config.skipFunctionBodies) opts |= ILanguage.OPTION_SKIP_FUNCTION_BODIES
  // enables parsing of code behind disabled preprocessor defines
  if (config.compilationDatabaseFilename.isEmpty && config.defines.isEmpty) opts |= ILanguage.OPTION_PARSE_INACTIVE_CODE

  def preprocessorStatements(file: Path, language: ILanguage): Iterable[IASTPreprocessorStatement] = {
    parse(file, language).map(t => preprocessorStatements(t)).getOrElse(Iterable.empty)
  }

  private def createScannerInfo(file: Path): ScannerInfo = {
    val additionalIncludes = if (FileDefaults.hasCppFileExtension(file.toString)) {
      parserConfig.systemIncludePathsCPP
    } else {
      parserConfig.systemIncludePathsC
    }
    val fileSpecificDefines  = parserConfig.definedSymbolsPerFile.getOrElse(file.toString, Map.empty)
    val fileSpecificIncludes = parserConfig.includesPerFile.getOrElse(file.toString, mutable.LinkedHashSet.empty)
    new ScannerInfo(
      (parserConfig.definedSymbols ++ fileSpecificDefines).asJava,
      fileSpecificIncludes.toArray ++ (parserConfig.userIncludePaths ++ additionalIncludes).map(_.toString).toArray
    )
  }

  def parse(file: Path, language: ILanguage): Option[IASTTranslationUnit] = {
    handleParseResult(file, parseInternal(file, language))
  }

  def parse(code: String, file: Path): Option[IASTTranslationUnit] = {
    handleParseResult(file, parseInternal(code, file))
  }

  private def handleParseResult(file: Path, result: ParseResult): Option[IASTTranslationUnit] = {
    result match {
      case ParseResult(translationUnit @ Some(_), Some(relativeFilePath), _) =>
        logger.info(s"Parsed '$relativeFilePath'")
        translationUnit
      case ParseResult(_, maybeRelativePath, maybeThrowable) =>
        val relativePath  = maybeRelativePath.getOrElse(file.toString)
        val throwableText = maybeThrowable.map(extractParseException).getOrElse("Unknown parse error!")
        logger.warn(s"Failed to parse '$relativePath': $throwableText")
        None
    }
  }

  private def prepareAndParse(file: Path, lang: ILanguage, fileContent: FileContent): ParseResult = {
    val relativeFilePath    = SourceFiles.toRelativePath(file.toString, config.inputPath)
    val fileContentProvider = new CustomFileContentProvider(headerFileFinder, file.toString, global)
    val scannerInfo         = createScannerInfo(file)
    safeParseInternal(fileContent, scannerInfo, fileContentProvider, lang, relativeFilePath)
  }

  private def parseInternal(file: Path, lang: ILanguage): ParseResult = {
    val fileContent = readFileAsFileContent(file)
    prepareAndParse(file, lang, fileContent)
  }

  private def parseInternal(code: String, file: Path): ParseResult = {
    val lang        = CdtParser.createParseLanguage(file, code, config)
    val fileContent = FileContent.create(file.toString, true, code.toCharArray)
    prepareAndParse(file, lang, fileContent)
  }

  private def safeParseInternal(
    fileContent: FileContent,
    scannerInfo: ScannerInfo,
    fileContentProvider: CustomFileContentProvider,
    lang: ILanguage,
    relativeFilePath: String
  ): ParseResult = {
    try {
      val translationUnit = lang.getASTTranslationUnit(fileContent, scannerInfo, fileContentProvider, null, opts, log)
      if (config.logProblems) logProblems(translationUnit)
      if (config.logPreprocessor) logPreprocessorStatements(translationUnit)
      ParseResult(Option(translationUnit), Some(relativeFilePath))
    } catch {
      case u: UnsupportedClassVersionError =>
        logger.error("c2cpg requires at least JRE-17 to run. Please check your Java Runtime Environment!", u)
        scala.sys.exit(1)
      case s: StackOverflowError =>
        /**   - Eclipse CDT’s C/C++ parser is heavily recursive (recursive-descent parsing, macro expansion, template
          *     instantiation, deep include chains). For certain real-world inputs this can create extremely deep call
          *     stacks and trigger a JVM `StackOverflowError`.
          *   - `StackOverflowError` is a `VirtualMachineError`, which Scala’s `NonFatal` does not catch. Without an
          *     explicit catch, a single problematic file would crash the whole process.
          *   - Catching it here lets `CdtParser` turn the failure into a `ParseResult`, log it, and continue parsing
          *     other files instead of aborting.
          *   - The risk is higher when parsing large header graphs, complex templates/macros, parsing inactive code
          *     (`OPTION_PARSE_INACTIVE_CODE`, which we use), or parsing headers twice (C and C++, which we do).
          */
        ParseResult(None, failure = Option(s))
      case NonFatal(e) =>
        ParseResult(None, failure = Option(e))
    }
  }

}
