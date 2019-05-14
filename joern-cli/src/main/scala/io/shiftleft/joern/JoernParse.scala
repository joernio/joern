package io.shiftleft.joern

import io.shiftleft.fuzzyc2cpg.Fuzzyc2Cpg
import org.slf4j.LoggerFactory

object JoernParse extends App {
  val DEFAULT_CPG_OUT_FILE = "cpg.bin.zip"

  private val logger = LoggerFactory.getLogger(getClass)

  parseConfig.foreach { config =>
    try {
      parse(config.inputPaths.toArray, config.outputPath)
    } catch {
      case exception: Exception =>
        logger.error("Failed to generate CPG.", exception)
        System.exit(1)
    }
    System.exit(0)
  }

  def parse(inputPaths: Array[String], outputPath: String): Unit = {
    new Fuzzyc2Cpg(outputPath).runAndOutput(inputPaths)
    Cpg2Scpg.run(outputPath)
  }

  case class Config(inputPaths: Seq[String], outputPath: String)
  def parseConfig: Option[Config] =
    new scopt.OptionParser[Config](getClass.getSimpleName) {
      arg[String]("<input-dir>")
        .unbounded()
        .text("source directories containing C/C++ code")
        .action((x, c) => c.copy(inputPaths = c.inputPaths :+ x))
      opt[String]("out")
        .text("output filename")
        .action((x, c) => c.copy(outputPath = x))

    }.parse(args, Config(List(), DEFAULT_CPG_OUT_FILE))

}
