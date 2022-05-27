package io.joern.console.cpgcreation

import io.joern.console.FrontendConfig

import java.nio.file.Path

case class PhpCpgGenerator(config: FrontendConfig, rootPath: Path) extends CpgGenerator {
  private lazy val command: Path = if (isWin) rootPath.resolve("php2cpg.bat") else rootPath.resolve("php2cpg")

  override def generate(inputPath: String, outputPath: String, namespaces: List[String]): Option[String] = {
    val arguments = Seq("create") ++ List(inputPath) ++ Seq("-o", outputPath) ++ config.cmdLineParams
    runShellCommand(command.toString, arguments).map(_ => outputPath)
  }

  override def isAvailable: Boolean =
    command.toFile.exists
}
