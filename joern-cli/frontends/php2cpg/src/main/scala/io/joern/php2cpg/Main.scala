package io.joern.php2cpg

import io.joern.x2cpg.{X2CpgConfig, X2CpgMain}
import io.joern.x2cpg.passes.frontend.{TypeRecoveryParserConfig, XTypeRecovery}
import io.joern.php2cpg.Frontend._
import scopt.OParser

/** Command line configuration parameters
  */
final case class Config(phpIni: Option[String] = None, phpParserBin: Option[String] = None)
    extends X2CpgConfig[Config]
    with TypeRecoveryParserConfig[Config] {
  def withPhpIni(phpIni: String): Config = {
    copy(phpIni = Some(phpIni)).withInheritedFields(this)
  }

  def withPhpParserBin(phpParserBin: String): Config = {
    copy(phpParserBin = Some(phpParserBin)).withInheritedFields(this)
  }
}

object Frontend {

  implicit val defaultConfig: Config = Config()

  val cmdLineParser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    import builder._
    OParser.sequence(
      programName("php2cpg"),
      opt[String]("php-ini")
        .action((x, c) => c.withPhpIni(x))
        .text("php.ini path used by php-parser. Defaults to php.ini shipped with Joern."),
      opt[String]("php-parser-bin")
        .action((x, c) => c.withPhpParserBin(x))
        .text("path to php-parser.phar binary. Defaults to php-parser shipped with Joern."),
      XTypeRecovery.parserOptions
    )
  }
}

object Main extends X2CpgMain(cmdLineParser, new Php2Cpg()) {
  def run(config: Config, php2Cpg: Php2Cpg): Unit = {
    php2Cpg.run(config)
  }
}
