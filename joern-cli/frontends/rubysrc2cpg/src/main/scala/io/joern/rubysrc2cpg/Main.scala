package io.joern.rubysrc2cpg

import io.joern.rubysrc2cpg.Frontend.*
import io.joern.x2cpg.astgen.AstGenConfig
import io.joern.x2cpg.passes.frontend.{TypeRecoveryParserConfig, XTypeRecovery, XTypeRecoveryConfig}
import io.joern.x2cpg.typestub.TypeStubConfig
import io.joern.x2cpg.utils.server.FrontendHTTPServer
import io.joern.x2cpg.{DependencyDownloadConfig, X2CpgConfig, X2CpgMain}
import scopt.OParser

import java.nio.file.Paths

final case class Config(downloadDependencies: Boolean = false, useTypeStubs: Boolean = true)
    extends X2CpgConfig[Config]
    with DependencyDownloadConfig[Config]
    with TypeRecoveryParserConfig[Config]
    with TypeStubConfig[Config]
    with AstGenConfig[Config] {

  override val astGenProgramName: String        = "ruby_ast_gen"
  override val astGenConfigPrefix: String       = "rubysrc2cpg"
  override val multiArchitectureBuilds: Boolean = true

  this.defaultIgnoredFilesRegex = List("spec", "tests?", "vendor", "db(\\\\|/)([\\w_]*)migrate([_\\w]*)").flatMap {
    directory =>
      List(s"(^|\\\\)$directory($$|\\\\)".r.unanchored, s"(^|/)$directory($$|/)".r.unanchored)
  }

  override def withDownloadDependencies(value: Boolean): Config = {
    copy(downloadDependencies = value).withInheritedFields(this)
  }

  override def withTypeStubs(value: Boolean): Config = {
    copy(useTypeStubs = value).withInheritedFields(this)
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

object Main extends X2CpgMain(cmdLineParser, new RubySrc2Cpg()) with FrontendHTTPServer[Config, RubySrc2Cpg] {

  override protected def newDefaultConfig(): Config = Config()

  def run(config: Config, rubySrc2Cpg: RubySrc2Cpg): Unit = {
    if (config.serverMode) { startup() }
    else { rubySrc2Cpg.run(config) }
  }
}
