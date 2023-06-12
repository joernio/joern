package io.joern.rubysrc2cpg

import io.joern.rubysrc2cpg.passes.{AstCreationPass, AstPackagePass, PackageResolverPass, ConfigPass}
import io.joern.rubysrc2cpg.utils.PackageTable
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.X2CpgFrontend
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}
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
      val astCreationPass = new AstCreationPass(config.inputPath, cpg, global, packageTableInfo)
      astCreationPass.createAndApply()
      downloadDependency(config.inputPath)
      new AstPackagePass(cpg, getGemEnv(), global, packageTableInfo, config.inputPath).createAndApply()
      new PackageResolverPass(cpg, packageTableInfo).createAndApply()
      new TypeNodePass(astCreationPass.allUsedTypes(), cpg).createAndApply()
    }
  }

  private def getGemEnv(): List[String] = {
    // fetch gem path where dependency file present
    Try("gem env gempath".!!) match {
      case Success(gemPath) =>
        gemPath.split(":").toList
      case Failure(exception) =>
        logger.error(s"Error While fetching gem path: ${exception.getMessage}")
        List.empty
    }
  }

  private def downloadDependency(inputPath: String): Unit = {
    if (Files.isRegularFile(Paths.get(s"$inputPath/Gemfile"))) {
      Try(s"bundle install --gemfile=$inputPath/Gemfile".!!) match {
        case Success(bundleOutput) =>
          logger.info(s"Dependency installed successfully: $bundleOutput")
        case Failure(exception) =>
          logger.error(s"Error while downloading dependency: ${exception.getMessage}")
      }
    }
  }
}
