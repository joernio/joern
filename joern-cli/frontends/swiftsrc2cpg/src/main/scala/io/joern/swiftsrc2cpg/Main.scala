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
  override val sharedConfig: X2CpgConfig.SharedConfig = X2CpgConfig.SharedConfig(),
  override val sharedTypeRecoveryConfig: TypeRecoveryParserConfig.Config = TypeRecoveryParserConfig.Config()
) extends X2CpgConfig[Config]
    with TypeRecoveryParserConfig {

  override def withSharedConfig(newSharedConfig: X2CpgConfig.SharedConfig): Config =
    copy(sharedConfig = newSharedConfig)

  override def withSharedTypeRecoveryConfig(newSharedConfig: TypeRecoveryParserConfig.Config): Config =
    copy(sharedTypeRecoveryConfig = newSharedConfig)

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

object Main extends X2CpgMain(new SwiftSrc2Cpg(), cmdLineParser) with FrontendHTTPServer {

  def run(config: frontend.ConfigType): Unit = {
    if (config.serverMode) { startup(); config.serverTimeoutSeconds.foreach(serveUntilTimeout) }
    else {
      val absPath = Paths.get(config.inputPath).toAbsolutePath.toString
      if (Environment.pathExists(absPath)) {
        frontend.run(config.withInputPath(absPath))
      } else {
        System.exit(1)
      }
    }
  }

}
