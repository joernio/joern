package io.joern.console.cpgcreation

import io.joern.console.FrontendConfig

import java.nio.file.Path

case class PythonSrcCpgGenerator(config: FrontendConfig, rootPath: Path) extends CpgGenerator {

  /** Generate a CPG for the given input path. Returns the output path, or None, if no CPG was generated.
    */
  override def generate(
    inputPath: String,
    outputPath: String = "cpg.bin.zip",
    namespaces: List[String] = List()
  ): Option[String] = {
    val pysrc2cpgsh = rootPath.resolve("pysrc2cpg").toString
    val arguments   = Seq(inputPath, "-o", outputPath) ++ config.cmdLineParams
    runShellCommand(pysrc2cpgsh, arguments).map(_ => outputPath)
  }

  override def isAvailable: Boolean = rootPath.resolve("pysrc2cpg").toFile.exists()
}
