package io.joern.gosrc2cpg

import io.joern.gosrc2cpg.Frontend.*
import io.joern.x2cpg.{X2CpgConfig, X2CpgMain}
import io.joern.x2cpg.utils.server.FrontendHTTPServer
import scopt.OParser

import java.nio.file.Paths

final case class Config(
  fetchDependencies: Boolean = false,
  includeIndirectDependencies: Boolean = false,
  override val genericConfig: X2CpgConfig.GenericConfig = X2CpgConfig.GenericConfig()
) extends X2CpgConfig[Config] {
  override def withGenericConfig(value: X2CpgConfig.GenericConfig): Config = copy(genericConfig = value)

  def withFetchDependencies(value: Boolean): Config = {
    copy(fetchDependencies = value)
  }

  def withIncludeIndirectDependencies(value: Boolean): Config = {
    copy(includeIndirectDependencies = value)
  }
}

object Frontend {
  val cmdLineParser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    import builder._
    OParser.sequence(
      programName("gosrc2cpg"),
      opt[Unit]("fetch-dependencies")
        .text("attempt to fetch dependencies for extra type information")
        .action((_, c) => c.withFetchDependencies(true)),
      opt[Unit]("include-indirect-dependencies")
        .text("try to fetch indirect dependencies as well, this flag works along with flag 'fetch-dependencies'")
        .action((_, c) => c.withFetchDependencies(true))
    )
  }

}

object Main extends X2CpgMain(new GoSrc2Cpg(), cmdLineParser)
