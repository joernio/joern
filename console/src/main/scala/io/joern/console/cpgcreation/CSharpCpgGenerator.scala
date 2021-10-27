package io.joern.console.cpgcreation

import io.joern.console.FrontendConfig

import java.nio.file.Path

/**
  * C# language frontend. Translates C# project files into code property graphs.
  * */
case class CSharpCpgGenerator(config: FrontendConfig, rootPath: Path) extends CpgGenerator {

  private val dotnetFrameworkOpt = "--dotnet-framework"
  private val dotnetCoreOpt = "--dotnet-core"

  /**
    * Generate a CPG for the given input path.
    * Returns the output path, or None, if no
    * CPG was generated.
    **/
  override def generate(inputPath: String,
                        outputPath: String = "cpg.bin.zip",
                        namespaces: List[String] = List()): Option[String] = {
    var arguments = Seq("-i", inputPath, "-o", outputPath) ++ config.cmdLineParams
    var command = rootPath.resolve("csharp2cpg.sh").toString

    if (!config.cmdLineParams.exists(param => param == dotnetFrameworkOpt || param == dotnetCoreOpt)) {
      System.err.println(
        s"Neither $dotnetFrameworkOpt or $dotnetCoreOpt were specified. Defaulting to $dotnetFrameworkOpt...")
      arguments = arguments :+ dotnetFrameworkOpt
    }

    if (System.getProperty("os.name").startsWith("Windows")) {
      command = "powershell"
      arguments = Seq(rootPath.resolve("csharp2cpg.ps1").toString) ++ arguments
    }
    runShellCommand(command, arguments).map(_ => outputPath)
  }

  override def isAvailable: Boolean = rootPath.resolve("csharp2cpg.sh").toFile.exists()

}
