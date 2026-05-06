package io.joern.console.cpgcreation

import io.joern.console.FrontendConfig

import java.nio.file.Path
import scala.util.Try

case class AbapSrcCpgGenerator(config: FrontendConfig, rootPath: Path) extends CpgGenerator {
  private lazy val command: Path =
    if (isWin) rootPath.resolve("abap2cpg.bat") else rootPath.resolve("abap2cpg")

  override def generate(inputPath: String, outputPath: String): Try[String] = {
    val arguments = List(inputPath) ++ Seq("-o", outputPath) ++ config.cmdLineParams
    runShellCommand(command.toString, arguments).map(_ => outputPath)
  }

  override def isAvailable: Boolean = command.toFile.exists

  override def isJvmBased = true
}
