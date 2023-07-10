package io.joern.rubysrc2cpg

import io.joern.rubysrc2cpg.passes.{AstCreationPass, AstPackagePass, ConfigFileCreationPass}
import io.joern.rubysrc2cpg.utils.{ExternalDependenciesResolver, PackageTable}
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.X2CpgFrontend
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.passes.callgraph.NaiveCallLinker
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.passes.CpgPassBase
import org.slf4j.LoggerFactory

import scala.util.Try

class RubySrc2Cpg extends X2CpgFrontend[Config] {

  private val logger = LoggerFactory.getLogger(this.getClass)

  val global = new Global()

  override def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config: Config) { (cpg, config) =>
      val packageTableInfo = new PackageTable()
      new MetaDataPass(cpg, Languages.RUBYSRC, config.inputPath).createAndApply()
      new ConfigFileCreationPass(cpg).createAndApply()
      ExternalDependenciesResolver.downloadDependencies(config, cpg, packageTableInfo)
      val astCreationPass = new AstCreationPass(config.inputPath, cpg, global, packageTableInfo)
      astCreationPass.createAndApply()
      TypeNodePass.withRegisteredTypes(astCreationPass.allUsedTypes(), cpg).createAndApply()
    }
  }
}

object RubySrc2Cpg {

  def postProcessingPasses(cpg: Cpg, config: Option[Config] = None): List[CpgPassBase] = List(new NaiveCallLinker(cpg))

}
