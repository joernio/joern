package io.joern.rubysrc2cpg

import io.joern.rubysrc2cpg.Frontend._
import io.joern.x2cpg.{X2CpgConfig, X2CpgMain}
import scopt.OParser

final case class Config(enableDependencyDownload: Boolean = false) extends X2CpgConfig[Config] {

  def withEnableDependencyDownload(value: Boolean): Config = {
    copy(enableDependencyDownload = value).withInheritedFields(this)
  }
}

private object Frontend {

  implicit val defaultConfig: Config = Config()

  val cmdLineParser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    import builder._
    OParser.sequence(
      programName("rubysrc2cpg"),
      opt[Unit]("enableDependencyDownload")
        .hidden()
        .action((_, c) => c.withEnableDependencyDownload(false))
        .text("enable dependency download of gemfile")
    )
  }
}

object Main extends X2CpgMain(cmdLineParser, new RubySrc2Cpg()) {
  def run(config: Config, rubySrc2Cpg: RubySrc2Cpg): Unit = {
    rubySrc2Cpg.run(config)
  }
}
