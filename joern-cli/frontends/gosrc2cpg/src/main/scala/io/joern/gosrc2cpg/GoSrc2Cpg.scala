package io.joern.gosrc2cpg

import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.X2CpgFrontend
import io.shiftleft.codepropertygraph.Cpg

import scala.util.Try

class GoSrc2Cpg extends X2CpgFrontend[Config] {

  def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config) { (cpg, config) => {} }
  }
}
