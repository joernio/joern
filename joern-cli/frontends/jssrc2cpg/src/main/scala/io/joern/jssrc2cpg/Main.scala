package io.joern.jssrc2cpg

import io.joern.jssrc2cpg.Frontend._
import io.joern.jssrc2cpg.utils.Environment
import io.joern.x2cpg.X2CpgConfig
import io.joern.x2cpg.X2CpgMain
import scopt.OParser

final case class Config(inputPath: String = "", outputPath: String = X2CpgConfig.defaultOutputPath)
    extends X2CpgConfig[Config] {

  override def withInputPath(inputPath: String): Config = copy(inputPath = inputPath)
  override def withOutputPath(x: String): Config        = copy(outputPath = x)
}

private object Frontend {
  implicit val defaultConfig: Config = Config()

  val cmdLineParser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    import builder.programName
    OParser.sequence(programName("jssrc2cpg"))
  }

}

object Main extends X2CpgMain(cmdLineParser, new JsSrc2Cpg()) {

  def run(config: Config, jssrc2cpg: JsSrc2Cpg): Unit = {
    if (Environment.allPathsExist(Set(config.inputPath)) && Environment.valid()) {
      jssrc2cpg.run(config)
    } else {
      System.exit(1)
    }
  }

}
