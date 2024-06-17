package io.joern.console.cpgcreation

import io.joern.console.FrontendConfig
import io.joern.x2cpg.frontendspecific.javasrc2cpg
import io.joern.x2cpg.passes.frontend.XTypeRecovery
import io.shiftleft.codepropertygraph.generated.Cpg

import java.nio.file.Path
import scala.util.Try

/** Source-based front-end for Java
  */
case class JavaSrcCpgGenerator(config: FrontendConfig, rootPath: Path) extends CpgGenerator {
  private lazy val command: Path = if (isWin) rootPath.resolve("javasrc2cpg.bat") else rootPath.resolve("javasrc2cpg")
  private var enableTypeRecovery = false
  private var disableDummyTypes  = false

  /** Generate a CPG for the given input path. Returns the output path, or None, if no CPG was generated.
    */
  override def generate(inputPath: String, outputPath: String = "cpg.bin"): Try[String] = {
    val arguments = config.cmdLineParams.toSeq ++ Seq(inputPath, "--output", outputPath)
    enableTypeRecovery = arguments.exists(_ == s"--${javasrc2cpg.ParameterNames.EnableTypeRecovery}")
    disableDummyTypes = arguments.exists(_ == s"--${XTypeRecovery.ParameterNames.NoDummyTypes}")
    runShellCommand(command.toString, arguments).map(_ => outputPath)
  }

  override def applyPostProcessingPasses(cpg: Cpg): Cpg = {
    if (enableTypeRecovery)
      javasrc2cpg.typeRecoveryPasses(cpg, disableDummyTypes).foreach(_.createAndApply())
    super.applyPostProcessingPasses(cpg)
  }

  override def isAvailable: Boolean =
    command.toFile.exists

  override def isJvmBased = true
}
