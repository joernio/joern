package io.joern.console.cpgcreation

import io.joern.console.FrontendConfig

import java.nio.file.Path
import scala.util.Try

case class RustCpgGenerator(config: FrontendConfig, rootPath: Path) extends CpgGenerator {
  private lazy val command: Path =
    if (isWin) rootPath.resolve("rust2cpg.bat") else rootPath.resolve("rust2cpg")

  override def generate(inputPath: String, outputPath: String): Try[String] = {
    val arguments = List(inputPath) ++ Seq("-o", outputPath) ++ config.cmdLineParams
    runShellCommand(command.toString, arguments).map(_ => outputPath)
  }

  override def isAvailable: Boolean = command.toFile.exists

  override def isJvmBased = true
}
