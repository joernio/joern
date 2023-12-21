package io.joern.gosrc2cpg

import io.joern.gosrc2cpg.Frontend.*
import io.joern.x2cpg.astgen.AstGenConfig
import io.joern.x2cpg.{X2CpgConfig, X2CpgMain}
import scopt.OParser

import java.nio.file.Paths

final case class Config(fetchDependencies: Boolean = false, includeIndirectDependencies: Boolean = false)
    extends X2CpgConfig[Config]
    with AstGenConfig[Config] {

  override val astGenProgramName: String  = "goastgen"
  override val astGenConfigPrefix: String = "gosrc2cpg"

  def withFetchDependencies(value: Boolean): Config = {
    copy(fetchDependencies = value).withInheritedFields(this)
  }

  def withIncludeIndirectDependencies(value: Boolean): Config = {
    copy(includeIndirectDependencies = value).withInheritedFields(this)
  }
}

object Frontend {
  implicit val defaultConfig: Config = Config()

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

object Main extends X2CpgMain(cmdLineParser, new GoSrc2Cpg()) {

  def run(config: Config, gosrc2cpg: GoSrc2Cpg): Unit = {
    val absPath = Paths.get(config.inputPath).toAbsolutePath.toString
    gosrc2cpg.run(config.withInputPath(absPath))
  }
}
