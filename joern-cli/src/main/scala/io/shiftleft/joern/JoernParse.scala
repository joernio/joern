package io.shiftleft.joern

import io.shiftleft.fuzzyc2cpg.FuzzyC2Cpg
import org.slf4j.LoggerFactory

object JoernParse extends App {
  val DEFAULT_CPG_OUT_FILE = "cpg.bin.zip"

  private val logger = LoggerFactory.getLogger(getClass)

  parseConfig.foreach { config =>
    try {
      parse(config.inputPaths.toArray, config.outputPath, config.enhance, config.dataFlow, config.semanticsFile)
    } catch {
      case exception: Exception =>
        logger.error("Failed to enhance CPG.", exception)
        System.exit(1)
    }
    System.exit(0)
  }

  def parse(inputPaths: Array[String],
            cpgFilename: String,
            enhance: Boolean,
            dataFlow: Boolean,
            semanticsFile: String): Unit = {
    new FuzzyC2Cpg(cpgFilename).runAndOutput(inputPaths)
    if (enhance) {
      Cpg2Scpg.run(cpgFilename, dataFlow, semanticsFile)
    }
  }

  case class Config(inputPaths: Seq[String],
                    outputPath: String,
                    enhance: Boolean,
                    dataFlow: Boolean,
                    semanticsFile: String)

  def parseConfig: Option[Config] =
    new scopt.OptionParser[Config](getClass.getSimpleName) {
      arg[String]("<input-dir>")
        .unbounded()
        .text("source directories containing C/C++ code")
        .action((x, c) => c.copy(inputPaths = c.inputPaths :+ x))
      opt[String]("out")
        .text("output filename")
        .action((x, c) => c.copy(outputPath = x))
      opt[Unit]("noenhance")
        .text("run language frontend but do not enhance the CPG to create an SCPG")
        .action((x, c) => c.copy(enhance = false))
      opt[Unit]("nodataflow")
        .text("do not perform data flow analysis")
        .action((x, c) => c.copy(dataFlow = false))
      opt[String]("semanticsfile")
        .text("data flow semantics file")
        .action((x, c) => c.copy(semanticsFile = x))

    }.parse(args, Config(List(), DEFAULT_CPG_OUT_FILE, true, true, CpgLoader.defaultSemanticsFile))

}
