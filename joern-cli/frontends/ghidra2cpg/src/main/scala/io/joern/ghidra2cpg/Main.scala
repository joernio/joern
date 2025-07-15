package io.joern.ghidra2cpg

import io.joern.ghidra2cpg.Frontend.*
import io.joern.x2cpg.X2CpgConfig.SharedConfig
import io.joern.x2cpg.{X2CpgConfig, X2CpgMain}
import scopt.OParser

/** Command line configuration parameters
  */
final case class Config(override val sharedConfig: SharedConfig = SharedConfig()) extends X2CpgConfig[Config] {
  override def withSharedConfig(newSharedConfig: SharedConfig): Config = copy(sharedConfig = newSharedConfig)
}

private object Frontend {

  implicit val defaultConfig: Config = Config()

  val cmdLineParser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    import builder.programName
    OParser.sequence(programName("ghidra2cpg"))
  }
}

object Main extends X2CpgMain(new Ghidra2Cpg(), cmdLineParser.asInstanceOf) {
  def run(config: frontend.ConfigType): Unit = {
    frontend.run(config)
  }
}
