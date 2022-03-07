package io.joern.console.cpgcreation

import io.joern.console.FrontendConfig

import java.nio.file.Path

case class PhpCpgGenerator(config: FrontendConfig, rootPath: Path) extends CpgGenerator {

  override def generate(inputPath: String, outputPath: String, namespaces: List[String]): Option[String] = {
    val command   = rootPath.resolve("php2cpg").toString
    val arguments = Seq("create") ++ List(inputPath) ++ Seq("-o", outputPath) ++ config.cmdLineParams
    runShellCommand(command, arguments).map(_ => outputPath)
  }

  override def isAvailable: Boolean = rootPath.resolve("php2cpg").toFile.exists()
}
