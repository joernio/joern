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
  downloadDependencies: Boolean = false,
  override val sharedConfig: X2CpgConfig.GenericConfig = X2CpgConfig.GenericConfig(),
  override val sharedTypeRecoveryConfig: TypeRecoveryParserConfig.Config = TypeRecoveryParserConfig.Config(),
  override val typeStubsFilePath: Option[String] = None
) extends X2CpgConfig[Config]
    with TypeRecoveryParserConfig
    with TypeStubsParserConfig
    with DependencyDownloadConfig {

  override def withSharedConfig(newSharedConfig: X2CpgConfig.GenericConfig): Config =
    copy(sharedConfig = newSharedConfig)

  override def withSharedTypeRecoveryConfig(newSharedConfig: TypeRecoveryParserConfig.Config): Config =
    copy(sharedTypeRecoveryConfig = newSharedConfig)

  def withPhpIni(phpIni: String): Config = {
    copy(phpIni = Some(phpIni))
  }

  def withPhpParserBin(phpParserBin: String): Config = {
    copy(phpParserBin = Some(phpParserBin))
  }

  override def withDownloadDependencies(downloadDependencies: Boolean): Config = {
    copy(downloadDependencies = downloadDependencies)
  }

  protected override def internalWithTypeStubsFilePath(typeStubsFilePath: String): Config = {
    copy(typeStubsFilePath = Some(typeStubsFilePath))
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

object Main extends X2CpgMain(new Php2Cpg(): Php2Cpg, cmdLineParser) with FrontendHTTPServer
