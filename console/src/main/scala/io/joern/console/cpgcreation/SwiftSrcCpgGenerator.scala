package io.joern.console.cpgcreation

import better.files.File
import io.joern.console.FrontendConfig
import io.joern.x2cpg.frontendspecific.swiftsrc2cpg
import io.joern.x2cpg.passes.frontend.XTypeRecoveryConfig
import io.shiftleft.codepropertygraph.generated.Cpg

import java.nio.file.Path
import scala.compiletime.uninitialized
import scala.util.Try

case class SwiftSrcCpgGenerator(config: FrontendConfig, rootPath: Path) extends CpgGenerator {
  private lazy val command: Path =
    if (isWin) rootPath.resolve("swiftsrc2cpg.bat") else rootPath.resolve("swiftsrc2cpg.sh")
  private var typeRecoveryConfig: XTypeRecoveryConfig = uninitialized

  /** Generate a CPG for the given input path. Returns the output path, or None, if no CPG was generated.
    */
  override def generate(inputPath: String, outputPath: String = "cpg.bin.zip"): Try[String] = {
    if (File(inputPath).isDirectory) {
      invoke(inputPath, outputPath)
    } else {
      withFileInTmpFile(inputPath) { dir =>
        invoke(dir.pathAsString, outputPath)
      }
    }
  }

  private def invoke(inputPath: String, outputPath: String): Try[String] = {
    val arguments = Seq(inputPath, "--output", outputPath) ++ config.cmdLineParams
    typeRecoveryConfig = XTypeRecoveryConfig.parse(config.cmdLineParams.toSeq)
    runShellCommand(command.toString, arguments).map(_ => outputPath)
  }

  override def isAvailable: Boolean =
    command.toFile.exists

  override def applyPostProcessingPasses(cpg: Cpg): Cpg = {
    swiftsrc2cpg.postProcessingPasses(cpg, typeRecoveryConfig).foreach(_.createAndApply())
    cpg
  }

  override def isJvmBased = true
}
