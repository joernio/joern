package io.joern.php2cpg

import io.joern.x2cpg.{X2CpgConfig, X2CpgMain}
import io.joern.php2cpg.Frontend._
import scopt.OParser

/** Command line configuration parameters
  */
final case class Config(phpIni: Option[String] = None) extends X2CpgConfig[Config] {
  def withPhpIni(phpIni: String): Config = {
    copy(phpIni = Some(phpIni)).withInheritedFields(this)
  }
}

private object Frontend {

  implicit val defaultConfig: Config = Config()

  val cmdLineParser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    import builder._
    OParser.sequence(
      programName("php2cpg"),
      opt[String]("php-ini")
        .action((x, c) => c.withPhpIni(x))
        .text("php.ini path used by php-parser. Defaults to php.ini shipped with Joern.")
    )
  }
}

object Main extends X2CpgMain(cmdLineParser, new Php2Cpg()) {
  def run(config: Config, php2Cpg: Php2Cpg): Unit = {
    php2Cpg.run(config)
  }
}
