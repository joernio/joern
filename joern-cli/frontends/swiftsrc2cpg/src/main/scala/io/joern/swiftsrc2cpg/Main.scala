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
  xcodeOutputPath: Option[Path] = None,
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

  def withXcodeOutput(xcodeOutputPath: Path): Config = {
    copy(xcodeOutputPath = Option(xcodeOutputPath))
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
      opt[Path]("xcode-output")
        .text("the path to the Xcode compiler debug output")
        .validate { path =>
          val file = path.toRealPath().toFile
          if (!file.isFile || !file.canRead) {
            failure(s"The Xcode compiler output file can not be read: '${file.toString}'")
          } else {
            success
          }
        }
        .action((path, c) => c.withSwiftBuild(true).withXcodeOutput(path))
    )
  }

}

object Main extends X2CpgMain(new SwiftSrc2Cpg(), cmdLineParser)
