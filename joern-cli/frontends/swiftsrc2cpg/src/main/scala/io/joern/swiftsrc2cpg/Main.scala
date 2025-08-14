package io.joern.swiftsrc2cpg

import io.joern.swiftsrc2cpg.Frontend.*
import io.joern.x2cpg.X2CpgConfig
import io.joern.x2cpg.X2CpgMain
import io.joern.x2cpg.passes.frontend.TypeRecoveryParserConfig
import io.joern.x2cpg.passes.frontend.XTypeRecoveryConfig
import io.joern.x2cpg.utils.Environment
import io.joern.x2cpg.utils.server.FrontendHTTPServer
import scopt.OParser

import java.nio.file.{Path, Paths}

final case class Config(
  defines: Set[String] = Set.empty,
  swiftBuild: Boolean = false,
  buildLogPath: Option[Path] = None,
  override val genericConfig: X2CpgConfig.GenericConfig = X2CpgConfig.GenericConfig(),
  override val typeRecoveryParserConfig: TypeRecoveryParserConfig.Config = TypeRecoveryParserConfig.Config()
) extends X2CpgConfig[Config]
    with TypeRecoveryParserConfig {

  override def withGenericConfig(value: X2CpgConfig.GenericConfig): Config = copy(genericConfig = value)

  override def withTypeRecoveryParserConfig(value: TypeRecoveryParserConfig.Config): Config =
    copy(typeRecoveryParserConfig = value)

  def withDefines(defines: Set[String]): Config = {
    copy(defines = defines)
  }

  def withSwiftBuild(swiftBuild: Boolean): Config = {
    copy(swiftBuild = swiftBuild)
  }

  def withBuildLogPath(buildLogPath: Path): Config = {
    copy(buildLogPath = Option(buildLogPath))
  }
}

object Frontend {
  implicit val defaultConfig: Config = Config()

  val cmdLineParser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    import builder.*
    OParser.sequence(
      programName("swiftsrc2cpg"),
      XTypeRecoveryConfig.parserOptionsForParserConfig,
      opt[String]("define")
        .unbounded()
        .text("define a name")
        .action((d, c) => c.withDefines(c.defines + d)),
      opt[Unit]("swift-build")
        .text("build the project to retrieve full Swift compiler type information")
        .action((path, c) => c.withSwiftBuild(true)),
      opt[Path]("build-log-path")
        .text("the path to the compiler debug output log file")
        .validate { path =>
          val file = path.toRealPath().toFile
          if (!file.isFile || !file.canRead) {
            failure(s"Unable to read compiler debug output log file: '${file.toString}'")
          } else {
            success
          }
        }
        .action((path, c) => c.withSwiftBuild(true).withBuildLogPath(path))
    )
  }

}

object Main extends X2CpgMain(new SwiftSrc2Cpg(), cmdLineParser)
