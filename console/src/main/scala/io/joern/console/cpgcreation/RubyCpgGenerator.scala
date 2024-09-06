package io.joern.console.cpgcreation

import io.joern.console.FrontendConfig
import io.joern.rubysrc2cpg.{Config, RubySrc2Cpg}
import io.shiftleft.codepropertygraph.generated.Cpg

import java.nio.file.Path
import scala.util.Try

case class RubyCpgGenerator(config: FrontendConfig, rootPath: Path) extends CpgGenerator {
  private lazy val command: Path = if (isWin) rootPath.resolve("rubysrc2cpg.bat") else rootPath.resolve("rubysrc2cpg")

  override def generate(inputPath: String, outputPath: String): Try[String] = {
    val arguments = List(inputPath) ++ Seq("-o", outputPath) ++ config.cmdLineParams
    runShellCommand(command.toString, arguments).map(_ => outputPath)
  }

  override def isAvailable: Boolean =
    command.toFile.exists

  override def isJvmBased = true

  override def applyPostProcessingPasses(cpg: Cpg): Cpg = {
    val rubyConfig = Config()
    RubySrc2Cpg.postProcessingPasses(cpg, rubyConfig).foreach(_.createAndApply())
    cpg
  }

}
