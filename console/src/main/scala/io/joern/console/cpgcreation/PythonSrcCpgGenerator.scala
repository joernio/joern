package io.joern.console.cpgcreation

import io.joern.console.FrontendConfig
import io.joern.x2cpg.frontendspecific.pysrc2cpg
import io.joern.x2cpg.frontendspecific.pysrc2cpg.*
import io.joern.x2cpg.passes.frontend.XTypeRecoveryConfig
import io.shiftleft.semanticcpg.layers.LayerCreator

import java.nio.file.Path
import scala.util.Try

case class PythonSrcCpgGenerator(config: FrontendConfig, rootPath: Path) extends CpgGenerator {
  private lazy val command: Path      = if (isWin) rootPath.resolve("pysrc2cpg.bat") else rootPath.resolve("pysrc2cpg")
  private lazy val typeRecoveryConfig = XTypeRecoveryConfig.parse(config.cmdLineParams.toSeq)

  /** Generate a CPG for the given input path. Returns the output path, or None, if no CPG was generated.
    */
  override def generate(inputPath: String, outputPath: String = "cpg.bin.zip"): Try[String] = {
    val arguments = Seq(inputPath, "-o", outputPath) ++ config.cmdLineParams
    runShellCommand(command.toString, arguments).map(_ => outputPath)
  }

  override def isAvailable: Boolean =
    command.toFile.exists

  override protected def postProcessingLayer: Option[LayerCreator] =
    Some(new pysrc2cpg.PythonPostProcessing(typeRecoveryConfig))

  override def isJvmBased = true
}
