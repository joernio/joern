package io.joern.console.cpgcreation

import io.joern.console.FrontendConfig

import java.nio.file.Path
import scala.util.Try

/** Proprietary language frontend for Python code.
  */
case class PyCpgGenerator(config: FrontendConfig, rootPath: Path) extends CpgGenerator {

  lazy val command = rootPath.resolve("py2cpg.sh")

  /** Generate a CPG for the given input path. Returns the output path, or None, if no CPG was generated.
    */
  override def generate(inputPath: String, outputPath: String = "cpg.bin.zip"): Try[String] = {
    val arguments = Seq("i", inputPath, "-o", outputPath) ++ config.cmdLineParams ++ Seq("-i", inputPath)

    runShellCommand(command.toString, arguments).map(_ => outputPath)
  }

  override def isAvailable: Boolean = command.toFile.exists()

  override def isJvmBased = false
}
