package io.joern.javasrc2cpg.util

import better.files.File
import com.github.javaparser.{JavaParser, ParserConfiguration}
import com.github.javaparser.ParserConfiguration.LanguageLevel
import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.Node.{NODE_BY_BEGIN_POSITION, Parsedness}
import io.joern.javasrc2cpg.util.Delombok.DelombokMode
import io.joern.javasrc2cpg.util.SourceParser.fileIfExists
import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.joern.x2cpg.SourceFiles
import io.shiftleft.utils.IOUtils
import org.slf4j.LoggerFactory

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Path
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional
import scala.util.Try
import scala.util.matching.Regex

class SourceParser(
  val relativeFilenames: List[String],
  analysisRoot: Path,
  typesRoot: Path,
  dirToDelete: Option[Path]
) {

  private val logger = LoggerFactory.getLogger(this.getClass)

  /** Parse the given file into a JavaParser CompliationUnit that will be used for creating the CPG AST.
    *
    * @param relativeFilename
    *   path to the input file relative to the project root.
    * @param saveFileContent
    *   if true, the raw text for the file is returned as the second item in the tuple.
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
          Try(IOUtils.readEntireFile(file.path))
            .orElse(Try(file.contentAsString(Charset.defaultCharset())))
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

  def cleanupDelombokOutput(): Unit = {
    dirToDelete.foreach(path => File(path).delete())
  }

}

object SourceParser {
  case class FileInfo(relativePath: Path, packageName: Option[String], usesLombok: Boolean)

  object FileInfo {

    private val PackageNameRegex: Regex = raw"package\s+([a-zA-Z$$_.]+)\s*;".r

    def getFileInfo(inputDir: Path, filename: String): Option[FileInfo] = {
      fileIfExists(filename).map { file =>
        val relativePath = inputDir.relativize(Path.of(filename))
        val content      = file.contentAsString

        val packageName = PackageNameRegex.findFirstMatchIn(content).map(_.group(1))

        val usesLombok = content.contains("lombok")

        new FileInfo(relativePath, packageName, usesLombok)
      }
    }
  }

  private val logger = LoggerFactory.getLogger(this.getClass)

  private def checkExists(file: File): Option[File] = {
    if (file.exists) {
      Option(file)
    } else {
      logger.warn(s"Attempting to open file, but it does not exist: ${file.pathAsString}")
      None
    }
  }
  private[util] def fileIfExists(path: Path): Option[File] = {
    val file = File(path)
    checkExists(file)
  }

  private[util] def fileIfExists(filename: String): Option[File] = {
    val file = File(filename)
    checkExists(file)
  }

  def apply(config: Config, filenamesOverride: Option[List[String]]): SourceParser = {
    val inputPath = Path.of(config.inputPath)

    val fileInfo = filenamesOverride
      .getOrElse(
        SourceFiles.determine(
          config.inputPath,
          JavaSrc2Cpg.sourceFileExtensions,
          ignoredDefaultRegex = Option(JavaSrc2Cpg.DefaultIgnoredFilesRegex),
          ignoredFilesRegex = Option(config.ignoredFilesRegex),
          ignoredFilesPath = Option(config.ignoredFiles)
        )
      )
      .flatMap(FileInfo.getFileInfo(inputPath, _))

    val usesLombok = fileInfo.exists(_.usesLombok)

    var dirToDelete: Option[Path] = None
    lazy val delombokResult       = Delombok.run(inputPath, fileInfo, config.delombokJavaHome)
    lazy val delombokDir = {
      dirToDelete = Option.when(delombokResult.isDelombokedPath)(delombokResult.path)
      delombokResult.path
    }

    val (analysisRoot, typesRoot) = Delombok.parseDelombokModeOption(config.delombokMode) match {
      case DelombokMode.Default if usesLombok =>
        logger.info(s"Analysing delomboked code as lombok dependency was found.")
        (delombokDir, delombokDir)

      case DelombokMode.Default => (inputPath, inputPath)

      case DelombokMode.NoDelombok => (inputPath, inputPath)

      case DelombokMode.TypesOnly => (inputPath, delombokDir)

      case DelombokMode.RunDelombok => (delombokDir, delombokDir)
    }

    new SourceParser(fileInfo.map(_.relativePath.toString), analysisRoot, typesRoot, dirToDelete)
  }
}
