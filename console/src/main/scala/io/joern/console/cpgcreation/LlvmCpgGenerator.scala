package io.joern.console.cpgcreation

import io.joern.console.FrontendConfig

import java.nio.file.Path
import scala.util.Try

/** Language frontend for LLVM. Translates LLVM bitcode into Code Property Graphs.
  */
case class LlvmCpgGenerator(config: FrontendConfig, rootPath: Path) extends CpgGenerator {

  /** Generate a CPG for the given input path. Returns the output path, or None, if no CPG was generated.
    */
  override def generate(inputPath: String, outputPath: String = "cpg.bin.zip"): Try[String] = {
    val command   = rootPath.resolve("llvm2cpg.sh").toString
    val arguments = Seq("--output", outputPath) ++ config.cmdLineParams ++ List(inputPath)
    runShellCommand(command, arguments).map(_ => outputPath)
  }

  override def isAvailable: Boolean = rootPath.resolve("llvm2cpg.sh").toFile.exists()

  override def isJvmBased = false
}
