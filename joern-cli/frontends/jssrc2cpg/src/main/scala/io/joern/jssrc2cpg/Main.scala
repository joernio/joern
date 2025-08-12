package io.joern.jssrc2cpg

import io.joern.jssrc2cpg.Frontend.*
import io.joern.x2cpg.passes.frontend.{TypeRecoveryParserConfig, XTypeRecovery, XTypeRecoveryConfig}
import io.joern.x2cpg.utils.Environment
import io.joern.x2cpg.{X2CpgConfig, X2CpgMain}
import io.joern.x2cpg.utils.server.FrontendHTTPServer
import scopt.OParser

import java.nio.file.Paths

final case class Config(
  tsTypes: Boolean = true,
  override val genericConfig: X2CpgConfig.GenericConfig = X2CpgConfig.GenericConfig(),
  override val typeRecoveryParserConfig: TypeRecoveryParserConfig.Config = TypeRecoveryParserConfig.Config()
) extends X2CpgConfig[Config]
    with TypeRecoveryParserConfig {
  override def withGenericConfig(value: X2CpgConfig.GenericConfig): Config = copy(genericConfig = value)

  override def withTypeRecoveryParserConfig(value: TypeRecoveryParserConfig.Config): Config =
    copy(typeRecoveryParserConfig = value)

  def withTsTypes(value: Boolean): Config = {
    copy(tsTypes = value)
  }

}

object Frontend {
  implicit val defaultConfig: Config = Config()

  val cmdLineParser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    import builder.*
    OParser.sequence(
      programName("jssrc2cpg"),
      opt[Unit]("no-tsTypes")
        .hidden()
        .action((_, c) => c.withTsTypes(false))
        .text("disable generation of types via Typescript"),
      XTypeRecoveryConfig.parserOptionsForParserConfig
    )
  }

}

object Main extends X2CpgMain(new JsSrc2Cpg(), cmdLineParser)
