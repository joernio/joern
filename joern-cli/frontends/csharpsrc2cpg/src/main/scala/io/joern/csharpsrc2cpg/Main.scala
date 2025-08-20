package io.joern.csharpsrc2cpg

import io.joern.csharpsrc2cpg.Frontend.{cmdLineParser, defaultConfig}
import io.joern.x2cpg.passes.frontend.{TypeRecoveryParserConfig, XTypeRecovery, XTypeRecoveryConfig}
import io.joern.x2cpg.utils.Environment
import io.joern.x2cpg.{DependencyDownloadConfig, X2CpgConfig, X2CpgMain}
import io.joern.x2cpg.utils.server.FrontendHTTPServer
import org.slf4j.LoggerFactory
import scopt.OParser

import java.nio.file.Paths

final case class Config(
  downloadDependencies: Boolean = false,
  useBuiltinSummaries: Boolean = true,
  externalSummaryPaths: Set[String] = Set.empty,
  override val genericConfig: X2CpgConfig.GenericConfig = X2CpgConfig.GenericConfig(),
  override val typeRecoveryParserConfig: TypeRecoveryParserConfig.Config = TypeRecoveryParserConfig.Config()
) extends X2CpgConfig[Config]
    with DependencyDownloadConfig
    with TypeRecoveryParserConfig {
  override def withGenericConfig(value: X2CpgConfig.GenericConfig): Config =
    copy(genericConfig = value)

  override def withTypeRecoveryParserConfig(value: TypeRecoveryParserConfig.Config): Config =
    copy(typeRecoveryParserConfig = value)

  override def withDownloadDependencies(value: Boolean): Config = {
    copy(downloadDependencies = value)
  }

  def withUseBuiltinSummaries(value: Boolean): Config = {
    copy(useBuiltinSummaries = value)
  }

  def withExternalSummaryPaths(paths: Set[String]): Config = {
    copy(externalSummaryPaths = paths)
  }

}

object Frontend {
  implicit val defaultConfig: Config = Config()

  val cmdLineParser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    import builder.*
    OParser.sequence(
      programName("csharpsrc2cpg"),
      DependencyDownloadConfig.parserOptions,
      XTypeRecoveryConfig.parserOptionsForParserConfig,
      opt[Unit]("disable-builtin-summaries")
        .text("do not use the built-in type summaries")
        .action((_, c) => c.withUseBuiltinSummaries(false)),
      opt[Seq[String]]("external-summary-paths")
        .text("where to look for external type summaries produced by DotNetAstGen (comma-separated list of paths)")
        .action((paths, c) => c.withExternalSummaryPaths(c.externalSummaryPaths ++ paths))
    )
  }

}

object Main extends X2CpgMain(new CSharpSrc2Cpg(), cmdLineParser)
