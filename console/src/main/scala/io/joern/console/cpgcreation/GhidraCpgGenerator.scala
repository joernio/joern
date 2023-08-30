package io.joern.console.cpgcreation

import io.joern.console.FrontendConfig

import java.nio.file.Path
import scala.util.Try

/** Language frontend for Ghidra - translates compiled binaries into Code Property Graphs.
  */
case class GhidraCpgGenerator(config: FrontendConfig, rootPath: Path) extends CpgGenerator {
  private lazy val command: Path = if (isWin) rootPath.resolve("ghidra2cpg.bat") else rootPath.resolve("ghidra2cpg")

  /** Generate a CPG for the given input path. Returns the output path, or None, if no CPG was generated.
    */
  override def generate(inputPath: String, outputPath: String = "cpg.bin"): Try[String] = {
    val arguments = Seq(inputPath, "--output", outputPath) ++ config.cmdLineParams
    runShellCommand(command.toString, arguments).map(_ => outputPath)
  }

  override def isAvailable: Boolean =
    command.toFile.exists

  override def isJvmBased = true
}
