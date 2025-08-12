package io.joern.rubysrc2cpg

import io.joern.rubysrc2cpg.Frontend.*
import io.joern.x2cpg.X2CpgConfig.GenericConfig
import io.joern.x2cpg.passes.frontend.{TypeRecoveryParserConfig, XTypeRecovery, XTypeRecoveryConfig}
import io.joern.x2cpg.typestub.TypeStubConfig
import io.joern.x2cpg.utils.server.FrontendHTTPServer
import io.joern.x2cpg.{DependencyDownloadConfig, X2CpgConfig, X2CpgMain}
import scopt.OParser

import java.nio.file.Paths

final case class Config(
  downloadDependencies: Boolean = false,
  useTypeStubs: Boolean = true,
  override val genericConfig: GenericConfig = GenericConfig(defaultIgnoredFilesRegex =
    List("spec", "tests?", "vendor", "db(\\\\|/)([\\w_]*)migrate([_\\w]*)").flatMap { directory =>
      List(s"(^|\\\\)$directory($$|\\\\)".r.unanchored, s"(^|/)$directory($$|/)".r.unanchored)
    }
  ),
  override val typeRecoveryParserConfig: TypeRecoveryParserConfig.Config = TypeRecoveryParserConfig.Config()
) extends X2CpgConfig[Config]
    with DependencyDownloadConfig
    with TypeRecoveryParserConfig
    with TypeStubConfig {

  override def withGenericConfig(value: X2CpgConfig.GenericConfig): Config = copy(genericConfig = value)

  override def withTypeRecoveryParserConfig(value: TypeRecoveryParserConfig.Config): Config =
    copy(typeRecoveryParserConfig = value)

  override def withDownloadDependencies(value: Boolean): Config = {
    copy(downloadDependencies = value)
  }

  override def withTypeStubs(value: Boolean): Config = {
    copy(useTypeStubs = value)
  }
}

private object Frontend {

  implicit val defaultConfig: Config = Config()

  val cmdLineParser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    import builder.*
    OParser.sequence(
      programName("rubysrc2cpg"),
      opt[Unit]("enable-file-content")
        .action((_, c) => c.withDisableFileContent(false))
        .text("Enable file content"),
      DependencyDownloadConfig.parserOptions,
      XTypeRecoveryConfig.parserOptionsForParserConfig,
      TypeStubConfig.parserOptions
    )
  }
}

object Main extends X2CpgMain(new RubySrc2Cpg(), cmdLineParser)
