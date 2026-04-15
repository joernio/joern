package io.joern.rust2cpg

import io.joern.rust2cpg.Frontend.*
import io.joern.x2cpg.{X2CpgConfig, X2CpgMain}
import scopt.OParser

final case class Config(override val genericConfig: X2CpgConfig.GenericConfig = X2CpgConfig.GenericConfig())
    extends X2CpgConfig[Config] {
  override def withGenericConfig(value: X2CpgConfig.GenericConfig): Config = copy(genericConfig = value)
}

private object Frontend {
  val cmdLineParser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    import builder._
    OParser.sequence(programName("rust2cpg"))
  }
}

object Main extends X2CpgMain(new Rust2Cpg(), cmdLineParser)
