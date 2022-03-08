package io.joern.solidity2cpg

import io.joern.x2cpg.{X2Cpg, X2CpgConfig}
import scopt.OParser

/** Command line configuration parameters
  */
final case class Config(inputPaths: Set[String] = Set.empty, outputPath: String = X2CpgConfig.defaultOutputPath)
    extends X2CpgConfig[Config] {

  override def withAdditionalInputPath(inputPath: String): Config =
    copy(inputPaths = inputPaths + inputPath)
  override def withOutputPath(x: String): Config = copy(outputPath = x)
}

/** Entry point for command line CPG creator
  */
object Main extends App {

  private val frontendSpecificOptions = {
    val builder = OParser.builder[Config]
    import builder.programName
    OParser.sequence(programName("solidity2cpg"))
  }

  X2Cpg.parseCommandLine(args, frontendSpecificOptions, Config()) match {
    case Some(config) =>
      if (config.inputPaths.size == 1) {
        val cpg = new Solidity2Cpg().createCpg(config.inputPaths.head, Some(config.outputPath))
        cpg.close()
      } else {
        println("This frontend requires exactly one input path")
        System.exit(1)
      }
    case None =>
      System.exit(1)
  }

}
