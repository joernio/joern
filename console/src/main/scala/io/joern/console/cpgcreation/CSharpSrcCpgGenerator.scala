package io.joern.console.cpgcreation

import io.joern.console.FrontendConfig
import java.nio.file.Path
import scala.util.Try

case class CSharpSrcCpgGenerator(config: FrontendConfig, rootPath: Path) extends CpgGenerator {
  private lazy val command: Path =
    if (isWin) rootPath.resolve("csharpsrc2cpg.bat") else rootPath.resolve("csharpsrc2cpg")
  private lazy val cmdLineArgs = config.cmdLineParams.toSeq

  override def generate(inputPath: String, outputPath: String = "cpg.bin"): Try[String] = {
    val arguments = cmdLineArgs ++ Seq(inputPath, "--output", outputPath)
    runShellCommand(command.toString, arguments).map(_ => outputPath)
  }

  override def isAvailable: Boolean = command.toFile.exists

  override def isJvmBased: Boolean = true
}
