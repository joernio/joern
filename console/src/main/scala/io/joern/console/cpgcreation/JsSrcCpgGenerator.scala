package io.joern.console.cpgcreation

import io.joern.console.FrontendConfig
import io.joern.jssrc2cpg.{Config, Frontend, JsSrc2Cpg}
import io.joern.x2cpg.X2Cpg
import io.shiftleft.codepropertygraph.Cpg

import java.nio.file.Path
import scala.util.Try

case class JsSrcCpgGenerator(config: FrontendConfig, rootPath: Path) extends CpgGenerator {
  private lazy val command: Path = if (isWin) rootPath.resolve("jssrc2cpg.bat") else rootPath.resolve("jssrc2cpg.sh")
  private var jsConfig: Option[Config] = None

  /** Generate a CPG for the given input path. Returns the output path, or None, if no CPG was generated.
    */
  override def generate(inputPath: String, outputPath: String = "cpg.bin.zip"): Try[String] = {
    val arguments = Seq(inputPath, "--output", outputPath) ++ config.cmdLineParams
    jsConfig = X2Cpg.parseCommandLine(arguments.toArray, Frontend.cmdLineParser, Config())
    runShellCommand(command.toString, arguments).map(_ => outputPath)
  }

  override def isAvailable: Boolean =
    command.toFile.exists

  override def applyPostProcessingPasses(cpg: Cpg): Cpg = {
    JsSrc2Cpg.postProcessingPasses(cpg, jsConfig).foreach(_.createAndApply())
    cpg
  }

  override def isJvmBased = true
}
