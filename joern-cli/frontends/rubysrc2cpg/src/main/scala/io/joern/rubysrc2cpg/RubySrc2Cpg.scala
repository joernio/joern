package io.joern.rubysrc2cpg

import better.files.File
import io.joern.rubysrc2cpg.passes.{AstCreationPass, AstPackagePass, ConfigFileCreationPass}
import io.joern.rubysrc2cpg.utils.PackageTable
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.X2CpgFrontend
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.passes.callgraph.NaiveCallLinker
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.joern.x2cpg.utils.ExternalCommand
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.passes.CpgPassBase
import org.slf4j.LoggerFactory
import sys.process._

import java.nio.file.{Files, Paths}
import scala.util.{Failure, Success, Try}

class RubySrc2Cpg extends X2CpgFrontend[Config] {

  private val logger = LoggerFactory.getLogger(this.getClass)

  val global = new Global()

  override def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config: Config) { (cpg, config) =>
      val packageTableInfo = new PackageTable()
      new MetaDataPass(cpg, Languages.RUBYSRC, config.inputPath).createAndApply()
      new ConfigFileCreationPass(cpg).createAndApply()
      downloadDependencies(config, cpg, packageTableInfo)
      val astCreationPass = new AstCreationPass(config.inputPath, cpg, global, packageTableInfo)
      astCreationPass.createAndApply()
      TypeNodePass.withRegisteredTypes(astCreationPass.allUsedTypes(), cpg).createAndApply()
    }
  }

  private def downloadDependencies(config: Config, cpg: Cpg, packageTable: PackageTable): Unit = {
    if (config.enableDependencyDownload && !scala.util.Properties.isWin) {
      if (checkDownloadPrerequisite()) {
        val tempDir = File.newTemporaryDirectory()
        try {
          downloadExternalGemFileDependency(config.inputPath, tempDir.toString())
          new AstPackagePass(cpg, tempDir.toString(), global, packageTable, config.inputPath).createAndApply()
        } catch {
          case ex: Exception =>
            println(s"Error while parsing dependency: ${ex.getMessage}")
        } finally {
          tempDir.delete()
        }
      }
    }
  }

  private def downloadExternalGemFileDependency(inputPath: String, tempPath: String): Unit = {
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

  private def checkDownloadPrerequisite(): Boolean = {
    val isRubyInstalled   = "ruby -v".! == 0
    val isBundleInstalled = "bundle -v".! == 0

    (isRubyInstalled, isBundleInstalled) match {
      case (false, true) =>
        logger.error("Skipping Dependency Download: Ruby is not installed.")
      case (true, false) =>
        logger.error("Skipping Dependency Download: Bundler is not installed.")
      case (false, false) =>
        logger.error("Skipping Dependency Download: Ruby and Bundler are not installed.")
      case _ =>
        logger.info("Ruby and Bundler are installed.")
    }

    isRubyInstalled && isBundleInstalled
  }
}

object RubySrc2Cpg {

  def postProcessingPasses(cpg: Cpg, config: Option[Config] = None): List[CpgPassBase] = List(new NaiveCallLinker(cpg))

}
