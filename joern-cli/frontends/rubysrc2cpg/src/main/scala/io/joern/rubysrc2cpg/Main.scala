package io.joern.rubysrc2cpg

import io.joern.rubysrc2cpg.Frontend.*
import io.joern.x2cpg.passes.frontend.{TypeRecoveryParserConfig, XTypeRecovery}
import io.joern.x2cpg.{X2CpgConfig, X2CpgMain}
import scopt.OParser

final case class Config(
  enableDependencyDownload: Boolean = false,
  antlrCacheMemLimit: Double = 0.6d,
  useDeprecatedFrontend: Boolean = false
) extends X2CpgConfig[Config]
    with TypeRecoveryParserConfig[Config] {

  def withEnableDependencyDownload(value: Boolean): Config = {
    copy(enableDependencyDownload = value).withInheritedFields(this)
  }

  def withAntlrCacheMemoryLimit(value: Double): Config = {
    copy(antlrCacheMemLimit = value).withInheritedFields(this)
  }

  def withUseDeprecatedFrontend(value: Boolean): Config = {
    copy(useDeprecatedFrontend = value).withInheritedFields(this)
  }
}

private object Frontend {

  implicit val defaultConfig: Config = Config()

  val cmdLineParser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    import builder.*
    OParser.sequence(
      programName("rubysrc2cpg"),
      opt[Unit]("enableDependencyDownload")
        .hidden()
        .action((_, c) => c.withEnableDependencyDownload(true))
        .text("enable dependency download for Unix System only"),
      opt[Double]("antlrCacheMemLimit")
        .hidden()
        .action((x, c) => c.withAntlrCacheMemoryLimit(x))
        .validate {
          case x if x < 0.3 =>
            failure(s"$x may result in too many evictions and reduce performance, try a value between 0.3 - 0.8.")
          case x if x > 0.8 =>
            failure(s"$x may result in too much memory usage and thrashing, try a value between 0.3 - 0.8.")
          case x =>
            success
        }
        .text("sets the heap usage threshold at which the ANTLR DFA cache is cleared during parsing (default 0.6)"),
      opt[Unit]("useDeprecatedFrontend")
        .action((_, c) => c.withUseDeprecatedFrontend(true))
        .text("uses the original (but deprecated) Ruby frontend (default false)"),
      XTypeRecovery.parserOptions
    )
  }
}

object Main extends X2CpgMain(cmdLineParser, new RubySrc2Cpg()) {
  def run(config: Config, rubySrc2Cpg: RubySrc2Cpg): Unit = {
    rubySrc2Cpg.run(config)
  }
}
