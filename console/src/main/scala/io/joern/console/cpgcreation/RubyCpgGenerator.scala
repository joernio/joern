package io.joern.console.cpgcreation

import io.joern.console.FrontendConfig
import io.joern.x2cpg.frontendspecific.rubysrc2cpg
import io.joern.x2cpg.passes.frontend.XTypeRecoveryConfig
import io.shiftleft.semanticcpg.layers.LayerCreator

import java.nio.file.Path
import scala.util.Try

case class RubyCpgGenerator(config: FrontendConfig, rootPath: Path) extends CpgGenerator {
  private lazy val command: Path = if (isWin) rootPath.resolve("rubysrc2cpg.bat") else rootPath.resolve("rubysrc2cpg")
  private lazy val cmdLineArgs   = config.cmdLineParams.toSeq
  private lazy val typeRecoveryConfig = XTypeRecoveryConfig.parse(cmdLineArgs)

  override def generate(inputPath: String, outputPath: String): Try[String] = {
    val arguments = List(inputPath) ++ Seq("-o", outputPath) ++ config.cmdLineParams
    runShellCommand(command.toString, arguments).map(_ => outputPath)
  }

  override def isAvailable: Boolean =
    command.toFile.exists

  override def isJvmBased = true

  override protected def postProcessingLayer: Option[LayerCreator] =
    Some(new rubysrc2cpg.RubyPostProcessing(typeRecoveryConfig))

}
