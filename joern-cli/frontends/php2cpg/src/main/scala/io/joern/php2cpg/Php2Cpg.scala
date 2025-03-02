package io.joern.php2cpg

import io.joern.php2cpg.parser.PhpParser
import io.joern.php2cpg.passes.*
import io.joern.php2cpg.utils.DependencyDownloader
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.shiftleft.semanticcpg.utils.ExternalCommand
import io.joern.x2cpg.{SourceFiles, X2CpgFrontend}
import io.shiftleft.codepropertygraph.generated.{Cpg, Languages}
import org.slf4j.LoggerFactory
import versionsort.VersionHelper

import scala.collection.mutable
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

class Php2Cpg extends X2CpgFrontend[Config] {

  private val logger = LoggerFactory.getLogger(this.getClass)

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
        new AstCreationPass(config, cpg, parser.get)(config.schemaValidation).createAndApply()
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
}
