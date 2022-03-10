package io.joern.c2cpg

import io.joern.x2cpg.{X2Cpg, X2CpgConfig}
import org.slf4j.LoggerFactory
import scopt.OParser

import scala.util.control.NonFatal

final case class Config(
  inputPaths: Set[String] = Set.empty,
  outputPath: String = X2CpgConfig.defaultOutputPath,
  includePaths: Set[String] = Set.empty,
  defines: Set[String] = Set.empty,
  includeComments: Boolean = false,
  logProblems: Boolean = false,
  logPreprocessor: Boolean = false,
  printIfDefsOnly: Boolean = false,
  includePathsAutoDiscovery: Boolean = true
) extends X2CpgConfig[Config] {

  override def withAdditionalInputPath(inputPath: String): Config = copy(inputPaths = inputPaths + inputPath)
  override def withOutputPath(x: String): Config                  = copy(outputPath = x)
}

object Main extends App {

  private val logger = LoggerFactory.getLogger(classOf[C2Cpg])

  val frontendSpecificOptions = {
    val builder = OParser.builder[Config]
    import builder._
    OParser.sequence(
      programName(classOf[C2Cpg].getSimpleName),
      opt[Unit]("include-comments")
        .text(s"includes all comments into the CPG")
        .action((_, c) => c.copy(includeComments = true)),
      opt[Unit]("log-problems")
        .text(s"enables logging of all parse problems while generating the CPG")
        .action((_, c) => c.copy(logProblems = true)),
      opt[Unit]("log-preprocessor")
        .text(s"enables logging of all preprocessor statements while generating the CPG")
        .action((_, c) => c.copy(logPreprocessor = true)),
      opt[Unit]("print-ifdef-only")
        .text(s"prints a comma-separated list of all preprocessor ifdef and if statements; does not create a CPG")
        .action((_, c) => c.copy(printIfDefsOnly = true)),
      opt[String]("include")
        .unbounded()
        .text("header include paths")
        .action((incl, c) => c.copy(includePaths = c.includePaths + incl)),
      opt[Unit]("no-include-auto-discovery")
        .text("disables auto discovery of header include paths")
        .action((_, c) => c.copy(includePathsAutoDiscovery = false)),
      opt[String]("define")
        .unbounded()
        .text("define a name")
        .action((d, c) => c.copy(defines = c.defines + d))
    )
  }

  X2Cpg.parseCommandLine(args, frontendSpecificOptions, Config()) match {
    case Some(config) if config.printIfDefsOnly =>
      try {
        new C2Cpg().printIfDefsOnly(config)
      } catch {
        case NonFatal(ex) =>
          logger.error("Failed to print preprocessor statements.", ex)
          System.exit(1)
      }
    case Some(config) =>
      try {
        val cpg = new C2Cpg().createCpg(config)
        cpg.close()
      } catch {
        case NonFatal(ex) =>
          logger.error("Failed to generate CPG.", ex)
          System.exit(1)
      }
    case None =>
      System.exit(1)
  }

}
