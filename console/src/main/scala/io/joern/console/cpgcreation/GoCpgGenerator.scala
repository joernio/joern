package io.joern.console.cpgcreation

import io.joern.console.FrontendConfig

import java.nio.file.Path

/** Language frontend for Go code. Translates Go source code into Code Property Graphs.
  */
case class GoCpgGenerator(config: FrontendConfig, rootPath: Path) extends CpgGenerator {

  /** Generate a CPG for the given input path. Returns the output path, or None, if no CPG was generated.
    */
  override def generate(
    inputPath: String,
    outputPath: String = "cpg.bin.zip",
    namespaces: List[String] = List()
  ): Option[String] = {
    var command   = rootPath.resolve("go2cpg.sh").toString
    var arguments = Seq("--output", outputPath) ++ config.cmdLineParams ++ Seq("generate") ++ List(inputPath)

    if (System.getProperty("os.name").startsWith("Windows")) {
      command = "powershell"
      arguments = Seq(rootPath.resolve("go2cpg.ps1").toString) ++ arguments
    }

    runShellCommand(command, arguments).map(_ => outputPath)
  }

  override def isAvailable: Boolean = rootPath.resolve("go2cpg.sh").toFile.exists()
}
