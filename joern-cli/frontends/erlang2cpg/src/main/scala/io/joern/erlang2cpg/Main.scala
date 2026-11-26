package io.joern.erlang2cpg

import io.joern.x2cpg.{X2CpgConfig, X2CpgMain}
import scopt.OParser

final case class Config(
  includes: Vector[String] = Vector.empty,
  macros: Vector[String] = Vector.empty,
  override val genericConfig: X2CpgConfig.GenericConfig = X2CpgConfig.GenericConfig()
) extends X2CpgConfig[Config] {
  override def withGenericConfig(value: X2CpgConfig.GenericConfig): Config = copy(genericConfig = value)
}

private object Frontend {
  val parser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    import builder.*
    OParser.sequence(
      programName("erlang2cpg"),
      opt[String]('I', "include")
        .unbounded()
        .valueName("<directory>")
        .action((dir, conf) => conf.copy(includes = conf.includes :+ dir)),
      opt[String]('D', "define")
        .unbounded()
        .valueName("<name[=term]>")
        .action((macroDef, conf) => conf.copy(macros = conf.macros :+ macroDef))
    )
  }
}

object Main extends X2CpgMain(new Erlang2Cpg(), Frontend.parser)
