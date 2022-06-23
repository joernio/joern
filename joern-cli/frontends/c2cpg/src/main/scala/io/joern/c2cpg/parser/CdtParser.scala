package io.joern.c2cpg.parser

import better.files.File
import io.joern.c2cpg.utils.IOUtils
import io.joern.c2cpg.Config
import org.eclipse.cdt.core.dom.ast.gnu.c.GCCLanguage
import org.eclipse.cdt.core.dom.ast.gnu.cpp.GPPLanguage
import org.eclipse.cdt.core.dom.ast.{IASTPreprocessorStatement, IASTTranslationUnit}
import org.eclipse.cdt.core.model.ILanguage
import org.eclipse.cdt.core.parser.{DefaultLogService, ScannerInfo}
import org.eclipse.cdt.internal.core.dom.parser.cpp.semantics.CPPVisitor
import org.slf4j.LoggerFactory

import java.nio.file.{NoSuchFileException, Path}
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

object CdtParser {

  private val logger = LoggerFactory.getLogger(classOf[CdtParser])

  case class ParseResult(
    translationUnit: Option[IASTTranslationUnit],
    preprocessorErrorCount: Int = 0,
    problems: Int = 0,
    failure: Option[Throwable] = None
  )

}

class CdtParser(config: Config) extends ParseProblemsLogger with PreprocessorStatementsLogger {

  import CdtParser._

  private val headerFileFinder = new HeaderFileFinder(config.inputPath)
  private val parserConfig     = ParserConfig.fromConfig(config)
  private val definedSymbols   = parserConfig.definedSymbols.asJava
  private val includePaths     = parserConfig.userIncludePaths
  private val log              = new DefaultLogService

  // enables parsing of code behind disabled preprocessor defines:
  private val opts: Int = ILanguage.OPTION_PARSE_INACTIVE_CODE

  private def createParseLanguage(file: Path): ILanguage = {
    if (FileDefaults.isCPPFile(file.toString)) {
      GPPLanguage.getDefault
    } else {
      GCCLanguage.getDefault
    }
  }

  private def createScannerInfo(file: Path): ScannerInfo = {
    val additionalIncludes =
      if (FileDefaults.isCPPFile(file.toString)) parserConfig.systemIncludePathsCPP
      else parserConfig.systemIncludePathsC
    new ScannerInfo(definedSymbols, (includePaths ++ additionalIncludes).map(_.toString).toArray)
  }

  private def parseInternal(file: Path): ParseResult = {
    val realPath = File(file)
    if (realPath.isRegularFile) { // handling potentially broken symlinks
      val fileContent         = IOUtils.readFileAsFileContent(realPath.path)
      val fileContentProvider = new CustomFileContentProvider(headerFileFinder)
      val lang                = createParseLanguage(realPath.path)
      val scannerInfo         = createScannerInfo(realPath.path)
      Try(lang.getASTTranslationUnit(fileContent, scannerInfo, fileContentProvider, null, opts, log)) match {
        case Failure(e) =>
          ParseResult(None, failure = Some(e))
        case Success(translationUnit) =>
          val problems = CPPVisitor.getProblems(translationUnit)
          if (parserConfig.logProblems) logProblems(problems.toList)
          if (parserConfig.logPreprocessor) logPreprocessorStatements(translationUnit)
          ParseResult(
            Some(translationUnit),
            preprocessorErrorCount = translationUnit.getPreprocessorProblemsCount,
            problems = problems.length
          )
      }
    } else {
      ParseResult(
        None,
        failure = Some(new NoSuchFileException(s"File '$realPath' does not exist. Check for broken symlinks!"))
      )
    }
  }

  def preprocessorStatements(file: Path): Iterable[IASTPreprocessorStatement] = {
    parse(file).map(t => preprocessorStatements(t)).getOrElse(Iterable.empty)
  }

  def parse(file: Path): Option[IASTTranslationUnit] = {
    val parseResult = parseInternal(file)
    parseResult match {
      case ParseResult(Some(t), c, p, _) =>
        logger.info(s"Parsed '${t.getFilePath}' ($c preprocessor error(s), $p problems)")
        Some(t)
      case ParseResult(_, _, _, maybeThrowable) =>
        logger.warn(
          s"Failed to parse '$file': ${maybeThrowable.map(extractParseException).getOrElse("Unknown parse error!")}"
        )
        None
    }
  }

}
