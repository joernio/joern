package io.joern.php2cpg

import io.joern.php2cpg.passes.AstCreationPass
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.X2CpgFrontend
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages

import scala.util.Try

class Php2Cpg extends X2CpgFrontend[Config] {
  override def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config: Config) { (cpg, config) =>
      new MetaDataPass(cpg, Languages.PHP, config.inputPath).createAndApply()
      new AstCreationPass(config.inputPath, cpg).createAndApply()
    }
  }
}
