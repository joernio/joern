package io.joern.ghidra2cpg

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

object Main extends App {
  private val frontendSpecificOptions = {
    val builder = OParser.builder[Config]
    import builder.programName
    OParser.sequence(programName("ghidra2cpg"))
  }

  X2Cpg.parseCommandLine(args, frontendSpecificOptions, Config()) match {
    case Some(config) =>
      createCpg(config)
    case None =>
      System.exit(1)
  }

  private def createCpg(config: Config): Unit = {
    if (config.inputPaths.size == 1) {
      val inputPath = config.inputPaths.head
      val cpg       = new Ghidra2Cpg().createCpg(inputPath, Some(config.outputPath))
      cpg.close()
    } else {
      println("This frontend requires exactly one input path")
      System.exit(1)
    }
  }

}
