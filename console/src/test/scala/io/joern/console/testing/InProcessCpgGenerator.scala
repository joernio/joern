package io.joern.console.testing

import io.joern.c2cpg.C2Cpg
import io.joern.console.FrontendConfig
import io.joern.console.cpgcreation.{
  CCpgGenerator,
  CpgGenerator,
  JsSrcCpgGenerator,
  SwiftSrcCpgGenerator,
  withFileInTmpFile
}
import io.joern.jssrc2cpg.JsSrc2Cpg
import io.joern.swiftsrc2cpg.SwiftSrc2Cpg
import io.joern.x2cpg.{X2Cpg, X2CpgConfig, X2CpgFrontend}
import io.shiftleft.codepropertygraph.generated.Cpg
import scopt.OParser

import java.nio.file.{Files, Path, Paths}
import scala.util.{Failure, Try}

/** A [[CpgGenerator]] for tests that runs the frontend inside the current JVM, instead of invoking the staged launcher
  * script in a separate process. This way, tests do not depend on a prior `<frontend>/stage`.
  *
  * @param newFrontend
  *   creates a fresh frontend instance for every invocation
  * @param parser
  *   the frontend's command line parser (used to interpret `config.cmdLineParams`)
  * @param config
  *   additional command line parameters
  * @param postProcessing
  *   the regular generator of that frontend, which is only used to apply its post-processing layer
  * @param wrapSingleFile
  *   if the input is a single file, copy it into a temporary directory first (as the real generators do)
  */
class InProcessCpgGenerator[C <: X2CpgConfig[C]](
  newFrontend: () => X2CpgFrontend { type ConfigType = C },
  parser: OParser[Unit, C],
  config: FrontendConfig,
  postProcessing: CpgGenerator,
  wrapSingleFile: Boolean
) extends CpgGenerator {

  override def isAvailable: Boolean = true

  override def isJvmBased: Boolean = false

  override def generate(inputPath: String, outputPath: String = "cpg.bin"): Try[String] = {
    if (wrapSingleFile && !Files.isDirectory(Paths.get(inputPath))) {
      withFileInTmpFile(inputPath) { dir => invoke(dir.toString, outputPath) }
    } else {
      invoke(inputPath, outputPath)
    }
  }

  private def invoke(inputPath: String, outputPath: String): Try[String] = {
    if (!Files.exists(Paths.get(inputPath))) {
      Failure(new AssertionError(s"Given path '$inputPath' does not exist"))
    } else {
      val frontend = newFrontend()
      try {
        val args = Array(inputPath, "--output", outputPath) ++ config.cmdLineParams
        X2Cpg.parseCommandLine(args, parser, frontend.defaultConfig) match {
          case None =>
            Failure(new IllegalArgumentException(s"Error parsing the command line: ${args.mkString(" ")}"))
          case Some(parsedConfig) =>
            Try(frontend.run(parsedConfig)).map(_ => outputPath)
        }
      } finally {
        frontend.close()
      }
    }
  }

  override def applyPostProcessingPasses(cpg: Cpg): Cpg = postProcessing.applyPostProcessingPasses(cpg)
}

object InProcessCpgGenerator {
  // only used to obtain the post-processing layers
  private val unusedRoot = Path.of("")

  def c(config: FrontendConfig = FrontendConfig()): CpgGenerator =
    new InProcessCpgGenerator(
      () => new C2Cpg(),
      io.joern.c2cpg.Frontend.cmdLineParser,
      config,
      CCpgGenerator(config, unusedRoot),
      wrapSingleFile = false
    )

  def jssrc(config: FrontendConfig = FrontendConfig()): CpgGenerator =
    new InProcessCpgGenerator(
      () => new JsSrc2Cpg(),
      io.joern.jssrc2cpg.Frontend.cmdLineParser,
      config,
      JsSrcCpgGenerator(config, unusedRoot),
      wrapSingleFile = true
    )

  def swiftsrc(config: FrontendConfig = FrontendConfig()): CpgGenerator =
    new InProcessCpgGenerator(
      () => new SwiftSrc2Cpg(),
      io.joern.swiftsrc2cpg.Frontend.cmdLineParser,
      config,
      SwiftSrcCpgGenerator(config, unusedRoot),
      wrapSingleFile = true
    )
}
