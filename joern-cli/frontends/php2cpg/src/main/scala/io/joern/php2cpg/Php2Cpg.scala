package io.joern.php2cpg

import io.joern.php2cpg.parser.PhpParser
import io.joern.php2cpg.passes.*
import io.joern.php2cpg.passes.SymbolSummaryPass.SymbolSummary
import io.joern.php2cpg.utils.DependencyDownloader
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.shiftleft.semanticcpg.utils.ExternalCommand
import io.joern.x2cpg.{SourceFiles, X2CpgFrontend}
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.codepropertygraph.generated.{Cpg, Languages}
import org.slf4j.LoggerFactory
import versionsort.VersionHelper

import scala.collection.mutable
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}
import scala.jdk.CollectionConverters.*

class Php2Cpg extends X2CpgFrontend {
  final override type ConfigType = Config
  override val defaultConfig = Config()

  private val logger = LoggerFactory.getLogger(this.getClass)

  private def isPhpVersionSupported: Boolean = {
    ExternalCommand.run(Seq("php", "--version")).logIfFailed().toTry match {
      case Success(s"PHP $version ($_" :: _) =>
        // PHP 7.1.0 and above is required by Composer, which is used by PHP Parser
        logger.info(s"Checking PHP installation: $version")
        val verPattern: Regex = """\b\d+\.\d+\.\d+\b""".r
        val verStr            = verPattern.findFirstIn(version).getOrElse(version)
        VersionHelper.compare(verStr, "7.1.0") >= 0
      case Success(output) =>
        logger.error(s"Unable to determine PHP version string from '$output'")
        false
      case Failure(exception) =>
        logger.error(s"Failed to run php --version: ${exception.getMessage}")
        false
    }
  }

  override def createCpg(config: Config): Try[Cpg] = {
    val errorMessages = mutable.ListBuffer[String]()

    if (!isPhpVersionSupported) {
      errorMessages.append("PHP version not supported. Is PHP 7.1.0 or above installed and available on your path?")
    }

    if (errorMessages.isEmpty) {
      PhpParser.withParser(config) { parserOption =>
        parserOption match {
          case Some(parser) =>
            withNewEmptyCpg(config.outputPath, config: Config) { (cpg, config) =>
              new MetaDataPass(cpg, Languages.PHP, config.inputPath).createAndApply()
              new DependencyPass(cpg, buildFiles(config)).createAndApply()
              if (config.downloadDependencies) {
                FileUtil.usingTemporaryDirectory("joern-php2cpg-deps") { dependencyDir =>
                  DependencyDownloader(cpg, config).download(dependencyDir)
                  // Parse dependencies and add high-level nodes to the CPG
                  new DependencySymbolsPass(cpg, dependencyDir).createAndApply()
                }
              }
              // The following block parses the code twice, once to summarize all symbols across various namespaces, and
              // twice to build the AST. This helps resolve symbols during the latter parse. Compared to parsing once, and
              // holding the AST in-memory, it was decided that parsing did not incur a significant speed impact, and lower
              // memory was prioritized.
              var buffer = Option.empty[Map[String, Seq[SymbolSummary]]]
              new SymbolSummaryPass(config, cpg, parser, summary => buffer = Option(summary)).createAndApply()
              new AstCreationPass(config, cpg, parser, buffer.getOrElse(Map.empty)).createAndApply()
              new AstParentInfoPass(cpg).createAndApply()
              TypeNodePass.withTypesFromCpg(cpg).createAndApply()
            }
          case None =>
            errorMessages.append("Could not initialize PhpParser")
            val errorOutput = (
              "Skipping AST creation as php/php-parser could not be executed." ::
                errorMessages.toList
            ).mkString("\n- ")

            logger.error(errorOutput)

            Failure(new RuntimeException("PhpParser could not be initialised"))
        }
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
}
