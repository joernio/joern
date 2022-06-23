package io.joern.php2cpg

import io.joern.x2cpg.{X2CpgConfig, X2CpgMain}
import io.joern.php2cpg.Frontend._
import scopt.OParser

/** Command line configuration parameters
  */
final case class Config(inputPath: String = "", outputPath: String = X2CpgConfig.defaultOutputPath)
    extends X2CpgConfig[Config] {

  override def withInputPath(inputPath: String): Config =
    copy(inputPath = inputPath)
  override def withOutputPath(x: String): Config = copy(outputPath = x)
}

private object Frontend {

  implicit val defaultConfig: Config = Config()

  val cmdLineParser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    import builder.programName
    OParser.sequence(programName("php2cpg"))
  }
}

object Main extends X2CpgMain(cmdLineParser, new Php2Cpg()) {
  def run(config: Config, php2Cpg: Php2Cpg): Unit = {
    php2Cpg.run(config)
  }
}
