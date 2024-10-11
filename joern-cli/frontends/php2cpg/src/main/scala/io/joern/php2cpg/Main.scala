package io.joern.php2cpg

import io.joern.php2cpg.Frontend.*
import io.joern.x2cpg.passes.frontend.*
import io.joern.x2cpg.{DependencyDownloadConfig, X2CpgConfig, X2CpgMain}
import io.joern.x2cpg.utils.server.FrontendHTTPServer
import scopt.OParser

/** Command line configuration parameters
  */
final case class Config(
  phpIni: Option[String] = None,
  phpParserBin: Option[String] = None,
  downloadDependencies: Boolean = false
) extends X2CpgConfig[Config]
    with TypeRecoveryParserConfig[Config]
    with TypeStubsParserConfig[Config]
    with DependencyDownloadConfig[Config] {

  def withPhpIni(phpIni: String): Config = {
    copy(phpIni = Some(phpIni)).withInheritedFields(this)
  }

  def withPhpParserBin(phpParserBin: String): Config = {
    copy(phpParserBin = Some(phpParserBin)).withInheritedFields(this)
  }

  override def withDownloadDependencies(downloadDependencies: Boolean): Config = {
    copy(downloadDependencies = downloadDependencies).withInheritedFields(this)
  }
}

object Frontend {

  implicit val defaultConfig: Config = Config()

  val cmdLineParser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    import builder.*
    OParser.sequence(
      programName("php2cpg"),
      opt[String]("php-ini")
        .action((x, c) => c.withPhpIni(x))
        .text("php.ini path used by php-parser. Defaults to php.ini shipped with Joern."),
      opt[String]("php-parser-bin")
        .action((x, c) => c.withPhpParserBin(x))
        .text("path to php-parser.phar binary. Defaults to php-parser shipped with Joern."),
      XTypeRecoveryConfig.parserOptionsForParserConfig,
      XTypeStubsParser.parserOptions,
      DependencyDownloadConfig.parserOptions
    )
  }
}

object Main extends X2CpgMain(cmdLineParser, new Php2Cpg()) with FrontendHTTPServer[Config, Php2Cpg] {

  override protected def newDefaultConfig(): Config = Config()

  def run(config: Config, php2Cpg: Php2Cpg): Unit = {
    if (config.serverMode) { startup() }
    else { php2Cpg.run(config) }
  }
}
