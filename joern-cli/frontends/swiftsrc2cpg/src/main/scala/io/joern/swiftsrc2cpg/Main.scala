package io.joern.swiftsrc2cpg

import io.joern.swiftsrc2cpg.Frontend.*
import io.joern.x2cpg.passes.frontend.{TypeRecoveryParserConfig, XTypeRecovery}
import io.joern.x2cpg.utils.Environment
import io.joern.x2cpg.{X2CpgConfig, X2CpgMain}
import scopt.OParser

import java.nio.file.Paths

final case class Config() extends X2CpgConfig[Config] with TypeRecoveryParserConfig[Config]

object Frontend {
  implicit val defaultConfig: Config = Config()

  val cmdLineParser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    import builder.*
    OParser.sequence(programName("swiftsrc2cpg"), XTypeRecovery.parserOptions)
  }

}

object Main extends X2CpgMain(cmdLineParser, new SwiftSrc2Cpg()) {

  def run(config: Config, swiftsrc2cpg: SwiftSrc2Cpg): Unit = {
    val absPath = Paths.get(config.inputPath).toAbsolutePath.toString
    if (Environment.pathExists(absPath)) {
      swiftsrc2cpg.run(config.withInputPath(absPath))
    } else {
      System.exit(1)
    }
  }

}
