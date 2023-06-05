package io.joern.php2cpg

import io.joern.x2cpg.{X2CpgConfig, X2CpgMain}
import io.joern.php2cpg.Frontend._
import scopt.OParser

/** Command line configuration parameters
  */
final case class Config(
  inputPath: String = "",
  outputPath: String = X2CpgConfig.defaultOutputPath,
  phpIni: Option[String] = None,
  excludeOverrides: Option[List[String]] = None
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
      programName("php2cpg"),
      opt[String]("php-ini")
        .action((x, c) => c.copy(phpIni = Some(x)))
        .text("php.ini path used by php-parser. Defaults to php.ini shipped with Joern."),
      opt[Seq[String]]("exclude-overrides")
        .valueName("regex1,regex2,...")
        .action((x, c) => c.copy(excludeOverrides = Some(x.toList)))
        .text("Comma separated list of regexes where matches exclude files or directories. Overrides default list.")
    )
  }
}

object Main extends X2CpgMain(cmdLineParser, new Php2Cpg()) {
  def run(config: Config, php2Cpg: Php2Cpg): Unit = {
    php2Cpg.run(config)
  }
}
