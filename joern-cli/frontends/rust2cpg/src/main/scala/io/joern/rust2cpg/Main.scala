package io.joern.rust2cpg

import io.joern.rust2cpg.Frontend.*
import io.joern.x2cpg.{X2CpgConfig, X2CpgMain}
import scopt.OParser

final case class Config(
  override val genericConfig: X2CpgConfig.GenericConfig = X2CpgConfig.GenericConfig(),
  noSysRoot: Boolean = false,
  noResolveCfg: Boolean = false,
  expandProcMacros: Boolean = false
) extends X2CpgConfig[Config] {
  override def withGenericConfig(value: X2CpgConfig.GenericConfig): Config = copy(genericConfig = value)

  def withNoSysRoot(value: Boolean): Config = copy(noSysRoot = value)

  def withNoResolveCfg(value: Boolean): Config = copy(noResolveCfg = value)

  def withExpandProcMacros(value: Boolean): Config = copy(expandProcMacros = value)
}

private object Frontend {
  val cmdLineParser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    import builder._
    OParser.sequence(
      programName("rust2cpg"),
      opt[Unit]("no-sysroot")
        .action((_, config) => config.withNoSysRoot(true))
        .text("Skip sysroot loading. Faster but will not resolve std symbols"),
      opt[Unit]("no-resolve-cfg")
        .action((_, config) => config.withNoResolveCfg(true))
        .text("Do not resolve #[cfg(...)] attributes"),
      opt[Unit]("expand-proc-macros")
        .action((_, config) => config.withExpandProcMacros(true))
        .text("Run build scripts and expand proc-macros")
    )
  }
}

object Main extends X2CpgMain(new Rust2Cpg(), cmdLineParser)
