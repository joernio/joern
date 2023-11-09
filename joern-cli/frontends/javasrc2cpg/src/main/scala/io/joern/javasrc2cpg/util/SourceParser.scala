package io.joern.javasrc2cpg.util

import better.files.File
import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.joern.javasrc2cpg.util.Delombok.DelombokMode
import io.joern.javasrc2cpg.util.Delombok.DelombokMode._
import com.github.javaparser.{JavaParser, ParserConfiguration}
import com.github.javaparser.ParserConfiguration.LanguageLevel
import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.Node.Parsedness
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters.RichOptional
import java.nio.file.Path
import java.nio.file.Paths
import java.util.concurrent.ConcurrentHashMap
import java.nio.charset.MalformedInputException
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets
import scala.util.Try
import scala.util.Success

class SourceParser private (originalInputPath: Path, analysisRoot: Path, typesRoot: Path) {

  private val logger = LoggerFactory.getLogger(this.getClass)

  /** Parse the given file into a JavaParser CompliationUnit that will be used for creating the CPG AST.
    *
    * @param relativeFilename
    *   path to the input file relative to the project root.
    */
  def parseAnalysisFile(
    relativeFilename: String,
    saveFileContent: Boolean
  ): Option[(CompilationUnit, Option[String])] = {
    val analysisFilename = analysisRoot.resolve(relativeFilename).toString
    // Need to store tokens for position information.
    fileIfExists(analysisFilename).flatMap { file =>
      val compilationUnit = parse(file, storeTokens = true)
      val fileContent = Option
        .when(saveFileContent) {
          Try(file.contentAsString(Charset.defaultCharset()))
            .orElse(Try(file.contentAsString(StandardCharsets.ISO_8859_1)))
            .toOption
        }
        .flatten

      compilationUnit.map(cu => (cu, fileContent))
    }
  }

  /** Parse the given file into a JavaParser CompliationUnit that will be used for reading type information. These
    * should not be used for determining the structure of the AST.
    *
    * @param relativeFilename
    *   path to the input file relative to the project root.
    */
  def parseTypesFile(relativeFilename: String): Option[CompilationUnit] = {
    val typesFilename = typesRoot.resolve(relativeFilename).toString
    fileIfExists(typesFilename).flatMap(parse(_, storeTokens = false))
  }

  def fileIfExists(filename: String): Option[File] = {
    val file = File(filename)

    Option.when(file.exists)(file)
  }

  def getTypesFileLines(relativeFilename: String): Try[Iterable[String]] = {
    val typesFilename = typesRoot.resolve(relativeFilename).toString
    Try(File(typesFilename).lines(Charset.defaultCharset()))
      .orElse(Try(File(typesFilename).lines(StandardCharsets.ISO_8859_1)))
  }

  def doesTypesFileExist(relativeFilename: String): Boolean = {
    File(typesRoot.resolve(relativeFilename)).isRegularFile
  }

  private def parse(file: File, storeTokens: Boolean): Option[CompilationUnit] = {
    val javaParserConfig =
      new ParserConfiguration()
        .setLanguageLevel(LanguageLevel.BLEEDING_EDGE)
        .setStoreTokens(storeTokens)
    val parseResult = new JavaParser(javaParserConfig).parse(file.toJava)

    parseResult.getProblems.asScala.toList match {
      case Nil => // Just carry on as usual
      case problems =>
        logger.warn(s"Encountered problems while parsing file ${file.name}:")
        problems.foreach { problem =>
          logger.warn(s"- ${problem.getMessage}")
        }
    }

    parseResult.getResult.toScala match {
      case Some(result) if result.getParsed == Parsedness.PARSED => Some(result)
      case _ =>
        logger.warn(s"Failed to parse file ${file.name}")
        None
    }
  }
}

object SourceParser {
  private val logger = LoggerFactory.getLogger(this.getClass)

  def apply(config: Config, hasLombokDependency: Boolean): SourceParser = {
    val canonicalInputPath = File(config.inputPath).canonicalPath
    val (analysisDir, typesDir) =
      getAnalysisAndTypesDirs(canonicalInputPath, config.delombokJavaHome, config.delombokMode, hasLombokDependency)

    new SourceParser(Path.of(canonicalInputPath), Path.of(analysisDir), Path.of(typesDir))
  }

  /** Implements the logic described in the option description for the "delombok-mode" option:
    *   - no-delombok: do not run delombok.
    *   - default: run delombok if a lombok dependency is found and analyse delomboked code.
    *   - types-only: run delombok, but use it for type information only
    *   - run-delombok: run delombok and analyse delomboked code
    *
    * @return
    *   the tuple (analysisRoot, typesRoot) where analysisRoot is used to locate source files for creating the AST and
    *   typesRoot is used for locating source files from which to extract type information.
    */
  private def getAnalysisAndTypesDirs(
    originalDir: String,
    delombokJavaHome: Option[String],
    delombokMode: Option[String],
    hasLombokDependency: Boolean
  ): (String, String) = {
    lazy val delombokDir = Delombok.run(originalDir, delombokJavaHome)

    Delombok.parseDelombokModeOption(delombokMode) match {
      case Default if hasLombokDependency =>
        logger.info(s"Analysing delomboked code as lombok dependency was found.")
        (delombokDir, delombokDir)

      case Default => (originalDir, originalDir)

      case NoDelombok => (originalDir, originalDir)

      case TypesOnly => (originalDir, delombokDir)

      case RunDelombok => (delombokDir, delombokDir)
    }
  }

}
