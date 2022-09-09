package io.joern.php2cpg

import io.joern.php2cpg.passes.AstCreationPass
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.X2CpgFrontend
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.joern.x2cpg.utils.ExternalCommand
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import org.slf4j.LoggerFactory

import scala.util.{Failure, Try}

class Php2Cpg extends X2CpgFrontend[Config] {
  private val logger = LoggerFactory.getLogger(this.getClass)

  private def isPhpInstalled: Boolean = {
    ExternalCommand.run("php --version", ".").isSuccess
  }

  override def createCpg(config: Config): Try[Cpg] = {
    if (isPhpInstalled) {
      withNewEmptyCpg(config.outputPath, config: Config) { (cpg, config) =>
        new MetaDataPass(cpg, Languages.PHP, config.inputPath).createAndApply()
        new AstCreationPass(config.inputPath, cpg).createAndApply()
      }
    } else {
      logger.error("Skipping AST creation as php could not be executed. Is PHP installed and available on your path?")
      Failure(new RuntimeException("php not found"))
    }

  }
}
