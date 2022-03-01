package io.joern.solidity2cpg

import better.files.File
import io.shiftleft.codepropertygraph.Cpg
import org.slf4j.LoggerFactory
import overflowdb.Config
import scopt.OParser

object Solidity2CpgConfig {
  def defaultOutputPath: String = "cpg.bin"
}

trait Solidity2CpgConfig[R] {
  def withAdditionalInputPath(inputPath: String): R
  def withOutputPath(x: String): R
}

object Solidity2Cpg {

  private val logger = LoggerFactory.getLogger(Solidity2Cpg.getClass)

  /** Parse commands line arguments in `args` using an X2Cpg command line parser, extended with the frontend specific
    * options in `frontendSpecific` with the initial configuration set to `initialConf`.
    *
    * On success, the configuration is returned wrapped into an Option. On failure, error messages are printed and
    * `None` is returned.
    */
  def parseCommandLine[R <: Solidity2CpgConfig[R]](
    args: Array[String],
    frontendSpecific: OParser[_, R],
    initialConf: R
  ): Option[R] = {
    val parser = commandLineParser(frontendSpecific)
    OParser.parse(parser, args, initialConf)
  }

  /** Create a command line parser that can be extended to add options specific for the frontend.
    */
  private def commandLineParser[R <: Solidity2CpgConfig[R]](frontendSpecific: OParser[_, R]): OParser[_, R] = {
    val builder = OParser.builder[R]
    import builder._
    OParser.sequence(
      arg[String]("input-dirs")
        .unbounded()
        .text("list of source files and/or source directories")
        .action((x, c) => c.withAdditionalInputPath(x)),
      opt[String]("output")
        .abbr("o")
        .text("output filename")
        .action { (x, c) =>
          c.withOutputPath(x)
        },
      help("help").text("display this help message"),
      frontendSpecific
    )
  }

  /** Create an empty CPG, backed by the file at `optionalOutputPath` or in-memory if `optionalOutputPath` is empty.
    */
  def newEmptyCpg(optionalOutputPath: Option[String] = None): Cpg = {
    val odbConfig = optionalOutputPath
      .map { outputPath =>
        val outFile = File(outputPath)
        if (outputPath != "" && outFile.exists) {
          logger.info("Output file exists, removing: " + outputPath)
          outFile.delete()
        }
        Config.withDefaults.withStorageLocation(outputPath)
      }
      .getOrElse {
        Config.withDefaults()
      }
    Cpg.withConfig(odbConfig)
  }

}
