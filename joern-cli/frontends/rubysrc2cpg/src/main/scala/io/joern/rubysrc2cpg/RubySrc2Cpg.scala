package io.joern.rubysrc2cpg

import better.files.File
import io.joern.rubysrc2cpg.passes.{
  AstCreationPass,
  AstPackagePass,
  ConfigFileCreationPass,
  ImportResolverPass,
  RubyTypeHintCallLinker,
  RubyTypeRecoveryPass
}
import io.joern.rubysrc2cpg.utils.PackageTable
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.X2CpgFrontend
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.passes.base.AstLinkerPass
import io.joern.x2cpg.passes.callgraph.NaiveCallLinker
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.joern.x2cpg.utils.ExternalCommand
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.passes.CpgPassBase
import org.slf4j.LoggerFactory

import java.nio.file.{Files, Paths}
import scala.util.{Failure, Success, Try}

class RubySrc2Cpg extends X2CpgFrontend[Config] {

  val global         = new Global()
  private val logger = LoggerFactory.getLogger(this.getClass)

  override def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config: Config) { (cpg, config) =>

      new MetaDataPass(cpg, Languages.RUBYSRC, config.inputPath).createAndApply()
      new ConfigFileCreationPass(cpg).createAndApply()
      if (config.enableDependencyDownload && !scala.util.Properties.isWin) {
        val tempDir = File.newTemporaryDirectory()
        try {
          downloadDependency(config.inputPath, tempDir.toString())
          new AstPackagePass(cpg, tempDir.toString(), global, RubySrc2Cpg.packageTableInfo, config.inputPath)
            .createAndApply()
        } finally {
          tempDir.delete()
        }
      }

      val astCreationPass = new AstCreationPass(cpg, global, RubySrc2Cpg.packageTableInfo, config)
      astCreationPass.createAndApply()
      TypeNodePass.withRegisteredTypes(astCreationPass.allUsedTypes(), cpg).createAndApply()
    }
  }

  private def downloadDependency(inputPath: String, tempPath: String): Unit = {
    if (Files.isRegularFile(Paths.get(s"${inputPath}${java.io.File.separator}Gemfile"))) {
      ExternalCommand.run(s"bundle config set --local path ${tempPath}", inputPath) match {
        case Success(configOutput) =>
          logger.info(s"Gem config successfully done: $configOutput")
        case Failure(exception) =>
          logger.error(s"Error while configuring Gem Path: ${exception.getMessage}")
      }
      val command = s"bundle install"
      ExternalCommand.run(command, inputPath) match {
        case Success(bundleOutput) =>
          logger.info(s"Dependency installed successfully: $bundleOutput")
        case Failure(exception) =>
          logger.error(s"Error while downloading dependency: ${exception.getMessage}")
      }
    }
  }
}

object RubySrc2Cpg {

  val packageTableInfo = new PackageTable()

  def postProcessingPasses(cpg: Cpg, config: Option[Config] = None): List[CpgPassBase] =
    List(
      // TODO commented below two passes, as waiting on Dependency download PR to get merged
      new ImportResolverPass(cpg, packageTableInfo),
      new RubyTypeRecoveryPass(cpg),
      new RubyTypeHintCallLinker(cpg),
      new NaiveCallLinker(cpg),

      // Some of passes above create new methods, so, we
      // need to run the ASTLinkerPass one more time
      new AstLinkerPass(cpg)
    )

}
