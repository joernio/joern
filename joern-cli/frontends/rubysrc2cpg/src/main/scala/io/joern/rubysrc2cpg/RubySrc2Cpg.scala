package io.joern.rubysrc2cpg

import io.joern.rubysrc2cpg.passes.{AstCreationPass, AstPackagePass, ConfigPass}
import io.joern.rubysrc2cpg.utils.PackageTable
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.X2CpgFrontend
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import org.slf4j.LoggerFactory

import java.io.File
import java.nio.file.{Files, Paths}
import scala.sys.process._
import scala.util.{Failure, Success, Try}

class RubySrc2Cpg extends X2CpgFrontend[Config] {

  private val tempDependencyDir = "tempExtDependency"

  private val logger = LoggerFactory.getLogger(this.getClass)

  val global = new Global()

  override def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config: Config) { (cpg, config) =>
      val packageTableInfo = new PackageTable()
      new MetaDataPass(cpg, Languages.RUBYSRC, config.inputPath).createAndApply()
      new ConfigPass(cpg, config.inputPath).createAndApply()
      if (config.enableDependencyDownload) {
        downloadDependency(config.inputPath)
        new AstPackagePass(cpg, tempDependencyDir, global, packageTableInfo, config.inputPath).createAndApply()
        deleteRecursively(new File(s"${config.inputPath}/$tempDependencyDir"))
      }
      val astCreationPass = new AstCreationPass(config.inputPath, cpg, global, packageTableInfo, tempDependencyDir)
      astCreationPass.createAndApply()
      new TypeNodePass(astCreationPass.allUsedTypes(), cpg).createAndApply()
    }
  }

  private def downloadDependency(inputPath: String): Unit = {
    if (Files.isRegularFile(Paths.get(s"$inputPath/Gemfile"))) {
      Try(s"bundle install --gemfile=$inputPath/Gemfile --path=$tempDependencyDir".!!) match {
        case Success(bundleOutput) =>
          logger.info(s"Dependency installed successfully: $bundleOutput")
        case Failure(exception) =>
          logger.error(s"Error while downloading dependency: ${exception.getMessage}")
      }
    }
  }

  private def deleteRecursively(file: File): Unit = {
    if (file.isDirectory) {
      file.listFiles.foreach(deleteRecursively)
    }
    if (file.exists && !file.delete) {
      throw new Exception(s"Unable to delete ${file.getAbsolutePath}")
    }
  }
}
