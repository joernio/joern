package io.joern.php2cpg

import io.joern.php2cpg.parser.PhpParser
import io.joern.php2cpg.passes.{
  AnyTypePass,
  AstCreationPass,
  AstParentInfoPass,
  ClosureRefPass,
  LocalCreationPass,
  PhpSetKnownTypesPass,
  PhpTypeRecoveryPassGenerator
}
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.X2CpgFrontend
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass, XTypeRecoveryConfig}
import io.joern.x2cpg.utils.ExternalCommand
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.CpgPassBase
import io.shiftleft.codepropertygraph.generated.Languages
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.util.{Failure, Try, Success}
import scala.util.matching.Regex

class Php2Cpg extends X2CpgFrontend[Config] {
  private val logger = LoggerFactory.getLogger(this.getClass)

  private def isPhpVersionSupported: Boolean = {
    // PHP 7.1.0 and above is required by Composer, which is used by PHP Parser
    val phpVersionRegex = new Regex("^PHP ([78]\\.[1-9]\\.[0-9]|[9-9]\\d\\.\\d\\.\\d)")
    val result          = ExternalCommand.run("php --version", ".")
    result match {
      case Success(listString) =>
        val phpVersionStr = listString.headOption.getOrElse("")
        logger.info(s"Checking PHP installation: $phpVersionStr")
        val matchResult = phpVersionRegex.findFirstIn(phpVersionStr)
        matchResult.isDefined
      case Failure(exception) =>
        logger.error(s"Failed to run php --version: ${exception.getMessage}")
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
}

object Php2Cpg {

  def postProcessingPasses(cpg: Cpg, config: Option[Config] = None): List[CpgPassBase] = {
    val typeRecoveryConfig = config
      .map(c => XTypeRecoveryConfig(c.typePropagationIterations, !c.disableDummyTypes))
      .getOrElse(XTypeRecoveryConfig(iterations = 3))
    List(new PhpSetKnownTypesPass(cpg)) ++ new PhpTypeRecoveryPassGenerator(cpg, typeRecoveryConfig).generate()
  }
}
