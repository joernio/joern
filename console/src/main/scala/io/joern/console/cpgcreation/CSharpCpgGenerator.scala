package io.joern.console.cpgcreation

import io.joern.console.FrontendConfig

import java.nio.file.Path
import scala.util.{Failure, Try}

/** C# language frontend. Translates C# project files into code property graphs.
  */
case class CSharpCpgGenerator(config: FrontendConfig, rootPath: Path) extends CpgGenerator {

  /** Generate a CPG for the given input path. Returns the output path, or None, if no CPG was generated.
    */
  override def generate(inputPath: String, outputPath: String = "cpg.bin.zip"): Try[String] = {
    if (commercialAvailable) {
      generateCommercial(inputPath, outputPath)
    } else if (ossAvailable) {
      generateOss(inputPath, outputPath)
    } else {
      Failure(new AssertionError("No C# language frontend present"))
    }
  }

  private def generateCommercial(inputPath: String, outputPath: String = "cpg.bin.zip"): Try[String] = {
    var arguments = Seq("-i", inputPath, "-o", outputPath) ++ config.cmdLineParams
    var command   = rootPath.resolve("csharp2cpg.sh").toString

    if (System.getProperty("os.name").startsWith("Windows")) {
      command = "powershell"
      arguments = Seq(rootPath.resolve("csharp2cpg.ps1").toString) ++ arguments
    }
    runShellCommand(command, arguments).map(_ => outputPath)
  }

  private def generateOss(inputPath: String, outputPath: String): Try[String] = {
    val command   = if (isWin) rootPath.resolve("csharpsrc2cpg.bat") else rootPath.resolve("csharpsrc2cpg")
    val arguments = config.cmdLineParams.toSeq ++ Seq(inputPath, "--output", outputPath)
    runShellCommand(command.toString, arguments).map(_ => outputPath)
  }

  override def isAvailable: Boolean = commercialAvailable || ossAvailable

  private def commercialAvailable: Boolean = rootPath.resolve("csharp2cpg.sh").toFile.exists()

  private def ossAvailable: Boolean = rootPath.resolve("csharpsrc2cpg").toFile.exists()

  override def isJvmBased = false
}
