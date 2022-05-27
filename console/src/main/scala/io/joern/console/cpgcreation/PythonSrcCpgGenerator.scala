package io.joern.console.cpgcreation

import io.joern.console.FrontendConfig

import java.nio.file.Path

case class PythonSrcCpgGenerator(config: FrontendConfig, rootPath: Path) extends CpgGenerator {
  private lazy val command: Path = if (isWin) rootPath.resolve("pysrc2cpg.bat") else rootPath.resolve("pysrc2cpg")

  /** Generate a CPG for the given input path. Returns the output path, or None, if no CPG was generated.
    */
  override def generate(
    inputPath: String,
    outputPath: String = "cpg.bin.zip",
    namespaces: List[String] = List()
  ): Option[String] = {
    val arguments = Seq(inputPath, "-o", outputPath) ++ config.cmdLineParams
    runShellCommand(command.toString, arguments).map(_ => outputPath)
  }

  override def isAvailable: Boolean =
    command.toFile.exists
}
