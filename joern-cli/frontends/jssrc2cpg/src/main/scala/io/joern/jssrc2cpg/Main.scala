package io.joern.jssrc2cpg

import io.joern.jssrc2cpg.utils.Environment
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.X2CpgConfig
import scopt.OParser

final case class Config(inputPaths: Set[String] = Set.empty, outputPath: String = X2CpgConfig.defaultOutputPath)
    extends X2CpgConfig[Config] {

  override def withAdditionalInputPath(inputPath: String): Config = copy(inputPaths = inputPaths + inputPath)
  override def withOutputPath(x: String): Config                  = copy(outputPath = x)
}

object Main extends App {

  private val frontendSpecificOptions = {
    val builder = OParser.builder[Config]
    import builder.programName
    OParser.sequence(programName("jssrc2cpg"))
  }

  X2Cpg.parseCommandLine(args, frontendSpecificOptions, Config()) match {
    case Some(config) if Environment.valid() =>
      new JsSrc2Cpg().run(config)
    case _ =>
      System.exit(1)
  }

}
