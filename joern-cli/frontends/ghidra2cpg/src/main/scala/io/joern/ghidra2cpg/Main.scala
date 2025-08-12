package io.joern.ghidra2cpg

import io.joern.ghidra2cpg.Frontend.*
import io.joern.x2cpg.X2CpgConfig.GenericConfig
import io.joern.x2cpg.utils.server.FrontendHTTPServer
import io.joern.x2cpg.{SingleThreadedFrontend, X2CpgConfig, X2CpgMain}
import scopt.OParser

import java.util.concurrent.ExecutorService

/** Command line configuration parameters
  */
final case class Config(override val genericConfig: GenericConfig = GenericConfig()) extends X2CpgConfig[Config] {
  override protected def withGenericConfig(value: GenericConfig): OwnType = copy(genericConfig = value)
}

private object Frontend {

  implicit val defaultConfig: Config = Config()

  val cmdLineParser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    import builder.programName
    OParser.sequence(programName("ghidra2cpg"))
  }
}

object Main extends X2CpgMain(new Ghidra2Cpg(), cmdLineParser) with SingleThreadedFrontend
