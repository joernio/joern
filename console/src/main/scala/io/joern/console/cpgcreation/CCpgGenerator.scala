package io.joern.console.cpgcreation

import io.shiftleft.console.FrontendConfig

import java.nio.file.Path

/**
  * Fuzzy C/C++ language frontend. Translates C/C++ source files
  * into code property graphs via fuzzy parsing.
  * */
case class CCpgGenerator(config: FrontendConfig, rootPath: Path) extends CpgGenerator {

  /**
    * Generate a CPG for the given input path.
    * Returns the output path, or None, if no
    * CPG was generated.
    **/
  override def generate(inputPath: String,
                        outputPath: String = "cpg.bin",
                        namespaces: List[String] = List()): Option[String] = {
    val command = rootPath.resolve("c2cpg.sh").toString
    val arguments = config.cmdLineParams.toSeq ++ Seq(inputPath, "--output", outputPath)
    runShellCommand(command, arguments).map(_ => outputPath)
  }

  override def isAvailable: Boolean = true
}
