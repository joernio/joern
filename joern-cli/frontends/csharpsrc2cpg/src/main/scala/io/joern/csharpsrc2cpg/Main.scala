package io.joern.csharpsrc2cpg

import io.joern.csharpsrc2cpg.Frontend.{cmdLineParser, defaultConfig}
import io.joern.x2cpg.astgen.AstGenConfig
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
  externalSummaryPaths: Set[String] = Set.empty
) extends X2CpgConfig[Config]
    with DependencyDownloadConfig[Config]
    with TypeRecoveryParserConfig[Config]
    with AstGenConfig[Config] {

  override val astGenProgramName: String  = "dotnetastgen"
  override val astGenConfigPrefix: String = "csharpsrc2cpg"

  override def withDownloadDependencies(value: Boolean): Config = {
    copy(downloadDependencies = value).withInheritedFields(this)
  }

  def withUseBuiltinSummaries(value: Boolean): Config = {
    copy(useBuiltinSummaries = value).withInheritedFields(this)
  }

  def withExternalSummaryPaths(paths: Set[String]): Config = {
    copy(externalSummaryPaths = paths).withInheritedFields(this)
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

object Main extends X2CpgMain(cmdLineParser, new CSharpSrc2Cpg()) with FrontendHTTPServer[Config, CSharpSrc2Cpg] {

  private val logger = LoggerFactory.getLogger(getClass)

  override protected def newDefaultConfig(): Config = Config()

  def run(config: Config, csharpsrc2cpg: CSharpSrc2Cpg): Unit = {
    if (config.serverMode) { startup() }
    else {
      val absPath = Paths.get(config.inputPath).toAbsolutePath.toString
      if (Environment.pathExists(absPath)) {
        csharpsrc2cpg.run(config.withInputPath(absPath))
      } else {
        logger.warn(s"Given path '$absPath' does not exist, skipping")
      }
    }
  }

}
