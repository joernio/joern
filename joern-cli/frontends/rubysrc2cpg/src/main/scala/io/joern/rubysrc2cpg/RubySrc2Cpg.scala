package io.joern.rubysrc2cpg

import better.files.File
import io.joern.rubysrc2cpg.passes.{AstCreationPass, AstPackagePass, ConfigPass}
import io.joern.rubysrc2cpg.utils.PackageTable
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.X2CpgFrontend
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.joern.x2cpg.utils.ExternalCommand
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import org.slf4j.LoggerFactory

import java.nio.file.{Files, Paths}
import scala.sys.process._
import scala.util.{Failure, Success, Try}

class RubySrc2Cpg extends X2CpgFrontend[Config] {

  private val logger = LoggerFactory.getLogger(this.getClass)

  val global = new Global()

  override def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config: Config) { (cpg, config) =>
      val packageTableInfo = new PackageTable()
      new MetaDataPass(cpg, Languages.RUBYSRC, config.inputPath).createAndApply()
      new ConfigPass(cpg, config.inputPath).createAndApply()
      if (config.enableDependencyDownload && !scala.util.Properties.isWin) {
        val tempDir = File.newTemporaryDirectory()
        try {
          downloadDependency(config.inputPath, tempDir.toString())
          new AstPackagePass(cpg, tempDir.toString(), global, packageTableInfo, config.inputPath).createAndApply()
        } finally {
          tempDir.delete()
        }
      }
      val astCreationPass = new AstCreationPass(config.inputPath, cpg, global, packageTableInfo)
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
