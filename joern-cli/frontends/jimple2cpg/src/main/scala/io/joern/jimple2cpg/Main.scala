package io.joern.jimple2cpg

import io.joern.jimple2cpg.Frontend._
import io.joern.x2cpg.{X2CpgConfig, X2CpgMain}
import scopt.OParser

/** Command line configuration parameters
  */
final case class Config(
  inputPath: String = "",
  outputPath: String = X2CpgConfig.defaultOutputPath,
  android: Option[String] = None
) extends X2CpgConfig[Config] {

  override def withInputPath(inputPath: String): Config =
    copy(inputPath = inputPath)
  override def withOutputPath(x: String): Config = copy(outputPath = x)
}

private object Frontend {

  implicit val defaultConfig: Config = Config()

  val cmdLineParser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    import builder._
    OParser.sequence(
      programName("jimple2cpg"),
      opt[String]("android")
        .text("Optional path to android.jar while processing apk file.")
        .action((android, config) => config.copy(android = Option(android)))
    )
  }
}

/** Entry point for command line CPG creator
  */
object Main extends X2CpgMain(cmdLineParser, new Jimple2Cpg()) {
  def run(config: Config, jimple2Cpg: Jimple2Cpg): Unit = {
    jimple2Cpg.run(config)
  }
}
