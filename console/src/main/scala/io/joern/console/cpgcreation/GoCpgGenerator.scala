package io.joern.console.cpgcreation

import io.joern.console.FrontendConfig

import java.nio.file.Path
import scala.util.Try

/** Language frontend for Go code. Translates Go source code into Code Property Graphs.
  */
case class GoCpgGenerator(config: FrontendConfig, rootPath: Path) extends CpgGenerator {
  private lazy val goSrc2CpgCommand: Path =
    if (isWin) rootPath.resolve("gosrc2cpg.bat") else rootPath.resolve("gosrc2cpg")

  /** Generate a CPG for the given input path. Returns the output path, or None, if no CPG was generated.
    */
  override def generate(inputPath: String, outputPath: String): Try[String] = {
    if (go2CpgAvailable()) go2CpgGenerate(inputPath, outputPath) else goSrc2CpgGenerate(inputPath, outputPath)
  }

  def go2CpgGenerate(inputPath: String, outputPath: String = "cpg.bin.zip"): Try[String] = {
    var command   = rootPath.resolve("go2cpg.sh").toString
    var arguments = Seq("--output", outputPath) ++ config.cmdLineParams ++ Seq("generate") ++ List(inputPath)

    if (System.getProperty("os.name").startsWith("Windows")) {
      command = "powershell"
      arguments = Seq(rootPath.resolve("go2cpg.ps1").toString) ++ arguments
    }

    runShellCommand(command, arguments).map(_ => outputPath)
  }

  def goSrc2CpgGenerate(inputPath: String, outputPath: String): Try[String] = {
    val arguments = List(inputPath) ++ Seq("-o", outputPath) ++ config.cmdLineParams
    runShellCommand(goSrc2CpgCommand.toString, arguments).map(_ => outputPath)
  }

  def go2CpgAvailable(): Boolean = {
    if (isWin) rootPath.resolve("go2cpg.ps1").toFile.exists() else rootPath.resolve("go2cpg.sh").toFile.exists()
  }

  def goSrc2CpgAvailable(): Boolean = {
    goSrc2CpgCommand.toFile.exists
  }

  override def isAvailable: Boolean = go2CpgAvailable() || goSrc2CpgAvailable()

  override def isJvmBased = false
}
