package io.joern.csharpsrc2cpg

import io.joern.csharpsrc2cpg.Frontend.{cmdLineParser, defaultConfig}
import io.joern.x2cpg.{X2CpgConfig, X2CpgMain}
import io.joern.x2cpg.passes.frontend.{TypeRecoveryParserConfig, XTypeRecovery}
import io.joern.x2cpg.utils.Environment
import org.slf4j.LoggerFactory
import scopt.OParser

import java.nio.file.Paths

final case class Config() extends X2CpgConfig[Config] with TypeRecoveryParserConfig[Config]

object Frontend {
  implicit val defaultConfig: Config = Config()

  val cmdLineParser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    import builder.*
    OParser.sequence(programName("csharpsrc2cpg"), XTypeRecovery.parserOptions)
  }

}

object Main extends X2CpgMain(cmdLineParser, new CSharpSrc2Cpg()) {

  private val logger = LoggerFactory.getLogger(getClass)

  def run(config: Config, csharpsrc2cpg: CSharpSrc2Cpg): Unit = {
    val absPath = Paths.get(config.inputPath).toAbsolutePath.toString
    if (Environment.pathExists(absPath)) {
      csharpsrc2cpg.run(config.withInputPath(absPath))
    } else {
      logger.warn(s"Given path '$absPath' does not exist, skipping")
    }
  }

}
