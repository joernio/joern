package io.joern.console.cpgcreation

import better.files.File
import io.joern.console.FrontendConfig
import io.joern.swiftsrc2cpg.{Config, Frontend, SwiftSrc2Cpg}
import io.joern.x2cpg.X2Cpg
import io.shiftleft.codepropertygraph.generated.Cpg

import java.nio.file.Path
import scala.util.Try

case class SwiftSrcCpgGenerator(config: FrontendConfig, rootPath: Path) extends CpgGenerator {
  private lazy val command: Path =
    if (isWin) rootPath.resolve("swiftsrc2cpg.bat") else rootPath.resolve("swiftsrc2cpg.sh")
  private var swiftConfig: Option[Config] = None

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
    swiftConfig = X2Cpg.parseCommandLine(arguments.toArray, Frontend.cmdLineParser, Config())
    runShellCommand(command.toString, arguments).map(_ => outputPath)
  }

  override def isAvailable: Boolean =
    command.toFile.exists

  override def applyPostProcessingPasses(cpg: Cpg): Cpg = {
    SwiftSrc2Cpg.postProcessingPasses(cpg, swiftConfig).foreach(_.createAndApply())
    cpg
  }

  override def isJvmBased = true
}
