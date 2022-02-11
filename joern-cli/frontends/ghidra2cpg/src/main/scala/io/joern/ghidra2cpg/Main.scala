package io.joern.ghidra2cpg

import io.shiftleft.x2cpg.{X2Cpg, X2CpgConfig}
import scopt.OParser

import java.io.File

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
      if (config.inputPaths.size == 1) {
        val inputFile = new File(config.inputPaths.head)
        new Ghidra2Cpg().createCpg(inputFile, Some(config.outputPath)).close()
      } else {
        println("This frontend requires exactly one input path")
        System.exit(1)
      }
    case None =>
      System.exit(1)
  }

}
