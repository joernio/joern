package io.joern.rubysrc2cpg

import io.joern.rubysrc2cpg.Frontend._
import io.joern.x2cpg.{X2CpgConfig, X2CpgMain}
import scopt.OParser

final case class Config(inputPath: String = "", outputPath: String = X2CpgConfig.defaultOutputPath)
    extends X2CpgConfig[Config] {

  override def withInputPath(inputPath: String): Config = copy(inputPath = inputPath)

  override def withOutputPath(x: String): Config = copy(outputPath = x)
}

private object Frontend {

  implicit val defaultConfig: Config = Config()

  val cmdLineParser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    import builder.programName
    OParser.sequence(programName("rubysrc2cpg"))
  }
}

object Main extends X2CpgMain(cmdLineParser, new RubySrc2Cpg()) {
  def run(config: Config, rubySrc2Cpg: RubySrc2Cpg): Unit = {
    rubySrc2Cpg.run(config)
  }
}
