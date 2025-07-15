package io.joern.swiftsrc2cpg

import io.joern.swiftsrc2cpg.Frontend.*
import io.joern.x2cpg.X2CpgConfig
import io.joern.x2cpg.X2CpgMain
import io.joern.x2cpg.passes.frontend.TypeRecoveryParserConfig
import io.joern.x2cpg.passes.frontend.XTypeRecoveryConfig
import io.joern.x2cpg.utils.Environment
import io.joern.x2cpg.utils.server.FrontendHTTPServer
import scopt.OParser

import java.nio.file.Paths

final case class Config(
  defines: Set[String] = Set.empty,
  override val genericConfig: X2CpgConfig.GenericConfig = X2CpgConfig.GenericConfig(),
  override val typeRecoveryParserConfig: TypeRecoveryParserConfig.Config = TypeRecoveryParserConfig.Config()
) extends X2CpgConfig[Config]
    with TypeRecoveryParserConfig {

  override def withGenericConfig(value: X2CpgConfig.GenericConfig): Config = copy(genericConfig = value)

  override def withTypeRecoveryParserConfig(value: TypeRecoveryParserConfig.Config): Config =
    copy(typeRecoveryParserConfig = value)

  def withDefines(defines: Set[String]): Config = {
    this.copy(defines = defines)
  }
}

object Frontend {
  implicit val defaultConfig: Config = Config()

  val cmdLineParser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    import builder.*
    OParser.sequence(
      programName("swiftsrc2cpg"),
      XTypeRecoveryConfig.parserOptionsForParserConfig,
      opt[String]("define")
        .unbounded()
        .text("define a name")
        .action((d, c) => c.withDefines(c.defines + d))
    )
  }

}

object Main extends X2CpgMain(new SwiftSrc2Cpg(), cmdLineParser)
