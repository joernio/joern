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

final case class Config(defines: Set[String] = Set.empty)
    extends X2CpgConfig[Config]
    with TypeRecoveryParserConfig[Config] {
  def withDefines(defines: Set[String]): Config = {
    this.copy(defines = defines).withInheritedFields(this)
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

object Main extends X2CpgMain(cmdLineParser, new SwiftSrc2Cpg()) with FrontendHTTPServer[Config, SwiftSrc2Cpg] {

  override protected def newDefaultConfig(): Config = Config()

  def run(config: Config, swiftsrc2cpg: SwiftSrc2Cpg): Unit = {
    if (config.serverMode) { startup(); config.serverTimeoutSeconds.foreach(serveUntilTimeout) }
    else {
      val absPath = Paths.get(config.inputPath).toAbsolutePath.toString
      if (Environment.pathExists(absPath)) {
        swiftsrc2cpg.run(config.withInputPath(absPath))
      } else {
        System.exit(1)
      }
    }
  }

}
