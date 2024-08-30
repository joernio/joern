package io.joern.jssrc2cpg

import io.joern.jssrc2cpg.Frontend.*
import io.joern.x2cpg.passes.frontend.{TypeRecoveryParserConfig, XTypeRecovery, XTypeRecoveryConfig}
import io.joern.x2cpg.utils.Environment
import io.joern.x2cpg.{X2CpgConfig, X2CpgMain}
import io.joern.x2cpg.utils.server.FrontendHTTPServer
import scopt.OParser

import java.nio.file.Paths

final case class Config(tsTypes: Boolean = true) extends X2CpgConfig[Config] with TypeRecoveryParserConfig[Config] {

  def withTsTypes(value: Boolean): Config = {
    copy(tsTypes = value).withInheritedFields(this)
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

object Main extends X2CpgMain(cmdLineParser, new JsSrc2Cpg()) with FrontendHTTPServer[Config, JsSrc2Cpg] {

  override protected def newDefaultConfig(): Config = Config()

  def run(config: Config, jssrc2cpg: JsSrc2Cpg): Unit = {
    if (config.serverMode) {
      startup(config)
    } else {
      val absPath = Paths.get(config.inputPath).toAbsolutePath.toString
      if (Environment.pathExists(absPath)) {
        jssrc2cpg.run(config.withInputPath(absPath))
      } else {
        System.exit(1)
      }
    }
  }

}
