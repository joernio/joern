package io.joern.console.cpgcreation

import io.joern.console.FrontendConfig

import java.nio.file.Path

case class PythonCpgGenerator(config: FrontendConfig, rootPath: Path) extends CpgGenerator {

  /** Generate a CPG for the given input path.
    * Returns the output path, or None, if no
    * CPG was generated.
    */
  override def generate(
      inputPath: String,
      outputPath: String = "cpg.bin.zip",
      namespaces: List[String] = List()
  ): Option[String] = {
    val py2cpgsh = rootPath.resolve("py2cpg.sh").toString
    val arguments = Seq(inputPath, "--output", outputPath) ++ config.cmdLineParams
    runShellCommand(py2cpgsh, arguments).map(_ => outputPath)
  }

  override def isAvailable: Boolean = rootPath.resolve("py2cpg.sh").toFile.exists()
}
