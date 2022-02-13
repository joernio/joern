package io.joern.console.cpgcreation

import io.joern.console.FrontendConfig

import java.nio.file.Path

/** C/C++ language frontend that translates C/C++ source files into code property graphs using Eclipse CDT parsing /
  * preprocessing.
  */
case class CCpgGenerator(config: FrontendConfig, rootPath: Path) extends CpgGenerator {
  lazy val command: Path = rootPath.resolve("c2cpg.sh")

  /** Generate a CPG for the given input path. Returns the output path, or None, if no CPG was generated.
    */
  override def generate(
    inputPath: String,
    outputPath: String = "cpg.bin",
    namespaces: List[String] = List()
  ): Option[String] = {
    val arguments = config.cmdLineParams.toSeq ++ Seq(inputPath, "--output", outputPath)
    runShellCommand(command.toString, arguments).map(_ => outputPath)
  }

  override def isAvailable: Boolean =
    command.toFile.exists

}
