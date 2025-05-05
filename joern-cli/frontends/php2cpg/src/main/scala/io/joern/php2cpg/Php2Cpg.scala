package io.joern.php2cpg

import io.joern.php2cpg.astcreation.AstCreator
import io.joern.php2cpg.datastructures.PhpProgramSummary
import io.joern.php2cpg.parser.{PhpParseResult, PhpParser}
import io.joern.php2cpg.passes.*
import io.joern.php2cpg.utils.DependencyDownloader
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.joern.x2cpg.utils.ConcurrentTaskUtil
import io.shiftleft.semanticcpg.utils.ExternalCommand
import io.joern.x2cpg.{SourceFiles, X2CpgFrontend}
import io.shiftleft.codepropertygraph.generated.{Cpg, Languages}
import io.shiftleft.semanticcpg.utils.FileUtil.PathExt
import org.slf4j.LoggerFactory
import versionsort.VersionHelper

import java.nio.file.Paths
import scala.collection.mutable
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

class Php2Cpg extends X2CpgFrontend[Config] {

  private val logger                               = LoggerFactory.getLogger(this.getClass)
  private val PhpSourceFileExtensions: Set[String] = Set(".php")

  private def isPhpVersionSupported: Boolean = {
    val result = ExternalCommand.run(Seq("php", "--version"), Some(".")).toTry
    result match {
      case Success(s"PHP $version ($_" :: _) =>
        // PHP 7.1.0 and above is required by Composer, which is used by PHP Parser
        logger.info(s"Checking PHP installation: $version")
        val verPattern: Regex = """\b\d+\.\d+\.\d+\b""".r
        val verStr            = verPattern.findFirstIn(version).getOrElse(version)
        VersionHelper.compare(verStr, "7.1.0") >= 0
      case Failure(exception) =>
        logger.error(s"Failed to run php --version: ${exception.getMessage}")
        false
      case x =>
        logger.error(s"Unable to determine PHP version string from '$x'")
        false
    }
  }

  override def createCpg(config: Config): Try[Cpg] = {
    val errorMessages = mutable.ListBuffer[String]()

    val parser = PhpParser.getParser(config)

    if (parser.isEmpty) {
      errorMessages.append("Could not initialize PhpParser")
    }
    if (!isPhpVersionSupported) {
      errorMessages.append("PHP version not supported. Is PHP 7.1.0 or above installed and available on your path?")
    }

    if (errorMessages.isEmpty) {
      withNewEmptyCpg(config.outputPath, config: Config) { (cpg, config) =>
        new MetaDataPass(cpg, Languages.PHP, config.inputPath).createAndApply()
        new DependencyPass(cpg, buildFiles(config)).createAndApply()
        if (config.downloadDependencies) {
          val dependencyDir = DependencyDownloader(cpg, config).download()
          // Parse dependencies and add high-level nodes to the CPG
          new DependencySymbolsPass(cpg, dependencyDir).createAndApply()
        }
        new AstCreationPass(cpg, parseFiles(config, parser)).createAndApply()
        new AstParentInfoPass(cpg).createAndApply()
        new AnyTypePass(cpg).createAndApply()
        TypeNodePass.withTypesFromCpg(cpg).createAndApply()
        LocalCreationPass.allLocalCreationPasses(cpg).foreach(_.createAndApply())
        new ClosureRefPass(cpg).createAndApply()
      }
    } else {
      val errorOutput = (
        "Skipping AST creation as php/php-parser could not be executed." ::
          errorMessages.toList
      ).mkString("\n- ")

      logger.error(errorOutput)

      Failure(new RuntimeException("php not found or version not supported"))
    }

  }

  private def buildFiles(config: Config): List[String] = {
    SourceFiles
      .determine(
        config.inputPath,
        Set(".json"),
        Option(config.defaultIgnoredFilesRegex),
        Option(config.ignoredFilesRegex),
        Option(config.ignoredFiles)
      )
      .filter(_.endsWith("composer.json"))
  }

  /** We need to feed the php parser big groups of file in order to speed up the parsing. Apparently it is some sort of
    * slow startup phase which makes single file processing prohibitively slow. On the other hand we need to be careful
    * to not choose too big chunks because:
    *   1. The argument length to the php executable has system dependent limits 2. We want to make use of multiple CPU
    *      cores for the rest of the CPG creation.
    */
  private[php2cpg] def parseFiles(config: Config, maybeParser: Option[PhpParser]): List[AstCreator] = {

    def parseResultToAstCreator(parseResult: PhpParseResult): Option[AstCreator] = {
      parseResult match {
        case PhpParseResult(fileName, Some(result), _) =>
          val relativeFilename = if (fileName == config.inputPath) {
            Paths.get(fileName).fileName
          } else {
            Paths.get(config.inputPath).relativize(Paths.get(fileName)).toString
          }
          Option(new AstCreator(relativeFilename, fileName, result, config.disableFileContent)(config.schemaValidation))
        case PhpParseResult(fileName, None, _) =>
          logger.warn(s"Could not parse file $fileName. Results will be missing!")
          None
      }
    }

    maybeParser match {
      case None => List.empty
      case Some(parser) =>
        val sourceFiles = SourceFiles
          .determine(
            config.inputPath,
            PhpSourceFileExtensions,
            ignoredFilesRegex = Option(config.ignoredFilesRegex),
            ignoredFilesPath = Option(config.ignoredFiles)
          )
          .toArray

        // Parse files concurrently in batches, creating AST creators from them
        val batchedParserTasks =
          sourceFiles
            .grouped(20)
            .map(fileNames => () => parser.parseFiles(fileNames).flatMap(parseResultToAstCreator).toSeq)

        val astCreators = ConcurrentTaskUtil
          .runUsingThreadPool(batchedParserTasks.iterator)
          .flatMap {
            case Failure(exception)   => logger.warn(s"Unable to parse PHP file batch, skipping - ", exception); Nil
            case Success(astCreators) => astCreators
          }

        // Pre-parse ASTs on a high level, not including method bodies, etc.
        val internalProgramSummary = ConcurrentTaskUtil
          .runUsingThreadPool(astCreators.map(x => () => x.summarize).iterator)
          .flatMap {
            case Failure(exception) => logger.warn(s"Unable to pre-parse PHP file, skipping - ", exception); None
            case Success(summary)   => Option(summary)
          }
          .foldLeft(PhpProgramSummary())(_ ++= _)

        // The result are AST creators with a reference to the program summary of all internal symbols (types/methods)
        astCreators.map(_.withSummary(internalProgramSummary))
    }
  }

}
