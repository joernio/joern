package io.joern.console.cpgcreation

import io.joern.console.FrontendConfig
import io.joern.x2cpg.frontendspecific.swiftsrc2cpg
import io.joern.x2cpg.passes.frontend.XTypeRecoveryConfig
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.codepropertygraph.generated.Cpg

import java.nio.file.{Path, Paths, Files}
import scala.util.Try

case class SwiftSrcCpgGenerator(config: FrontendConfig, rootPath: Path) extends CpgGenerator {
  private lazy val command: Path =
    if (isWin) rootPath.resolve("swiftsrc2cpg.bat") else rootPath.resolve("swiftsrc2cpg.sh")

  private lazy val typeRecoveryConfig = XTypeRecoveryConfig.parse(config.cmdLineParams.toSeq)

  /** Generate a CPG for the given input path. Returns the output path, or None, if no CPG was generated.
    */
  override def generate(inputPath: String, outputPath: String = "cpg.bin.zip"): Try[String] = {
    if (Files.isDirectory(Paths.get(inputPath))) {
      invoke(inputPath, outputPath)
    } else {
      withFileInTmpFile(inputPath) { dir =>
        invoke(dir.toString, outputPath)
      }
    }
  }

  private def invoke(inputPath: String, outputPath: String): Try[String] = {
    val arguments = Seq(inputPath, "--output", outputPath) ++ config.cmdLineParams
    runShellCommand(command.toString, arguments).map(_ => outputPath)
  }

  override def isAvailable: Boolean =
    Files.exists(command)

  override def applyPostProcessingPasses(cpg: Cpg): Cpg = {
    swiftsrc2cpg.postProcessingPasses(cpg, typeRecoveryConfig).foreach(_.createAndApply())
    cpg
  }

  override def isJvmBased = true
}
