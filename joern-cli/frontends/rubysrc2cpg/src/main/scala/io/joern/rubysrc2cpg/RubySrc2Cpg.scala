package io.joern.rubysrc2cpg

import io.joern.rubysrc2cpg.passes.{AstCreationPass, AstPackagePass, PackageResolverPass}
import io.joern.rubysrc2cpg.utils.PackageTable
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.X2CpgFrontend
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import org.slf4j.LoggerFactory
import scala.sys.process._

import scala.collection.mutable.ListBuffer
import scala.util.Try

class RubySrc2Cpg extends X2CpgFrontend[Config] {

  private val logger = LoggerFactory.getLogger(this.getClass)

  val global = new Global()

  override def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config: Config) { (cpg, config) =>
      val packageTableInfo = new PackageTable()
      new MetaDataPass(cpg, Languages.RUBYSRC, config.inputPath).createAndApply()
      val astCreationPass = new AstCreationPass(config.inputPath, cpg, global, packageTableInfo)
      astCreationPass.createAndApply()
      val gemPaths = getGemEnv()
      new AstPackagePass(cpg, gemPaths, global, packageTableInfo, config.inputPath).createAndApply()
      new PackageResolverPass(cpg, packageTableInfo).createAndApply()
      new TypeNodePass(astCreationPass.allUsedTypes(), cpg).createAndApply()
    }
  }

  def getGemEnv(): List[String] = {
    val gemEnvOutput = "gem env gempath".!!
    gemEnvOutput.split(":").toList
  }
}
