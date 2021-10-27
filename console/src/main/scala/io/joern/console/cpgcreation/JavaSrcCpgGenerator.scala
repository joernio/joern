package io.joern.console.cpgcreation

import io.joern.console.FrontendConfig

import java.nio.file.Path

/**
  * Source-based front-end for Java
  */
case class JavaSrcCpgGenerator(config: FrontendConfig, rootPath: Path) extends CpgGenerator {

  /**
    * Generate a CPG for the given input path.
    * Returns the output path, or None, if no
    * CPG was generated.
    **/
  override def generate(inputPath: String,
                        outputPath: String = "cpg.bin",
                        namespaces: List[String] = List()): Option[String] = {
    val command = rootPath.resolve("javasrc2cpg").toString
    val arguments = config.cmdLineParams.toSeq ++ Seq(inputPath, "--output", outputPath)
    runShellCommand(command, arguments).map(_ => outputPath)
  }

  override def isAvailable: Boolean = rootPath.resolve("javasrc2cpg").toFile.exists()
}
