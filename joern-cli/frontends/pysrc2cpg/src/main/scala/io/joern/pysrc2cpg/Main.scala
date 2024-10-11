package io.joern.pysrc2cpg

import io.joern.pysrc2cpg.Frontend.cmdLineParser
import io.joern.x2cpg.X2CpgMain
import io.joern.x2cpg.passes.frontend.XTypeRecoveryConfig
import io.joern.x2cpg.utils.server.FrontendHTTPServer
import scopt.OParser

import java.nio.file.Paths

private object Frontend {
  val cmdLineParser: OParser[Unit, Py2CpgOnFileSystemConfig] = {
    val builder = OParser.builder[Py2CpgOnFileSystemConfig]
    import builder._
    // Defaults for all command line options are specified in Py2CpgOFileSystemConfig
    // because Scopt is a shit library.
    OParser.sequence(
      programName("pysrc2cpg"),
      opt[String]("venvDir")
        .hidden() // deprecated; use venvDirs instead. Left this here to not break existing scripts.
        .text("Virtual environment directory. If not absolute it is interpreted relative to input-dir.")
        .action((dir, config) => config.withVenvDir(Paths.get(dir))),
      opt[Seq[String]]("venvDirs")
        .text("Virtual environment directories. If not absolute they are interpreted relative to input-dir.")
        .action((value, config) => config.withVenvDirs(value.map(Paths.get(_)))),
      opt[Boolean]("ignoreVenvDir")
        .text("Specifies whether venv-dir is ignored. Default to true.")
        .action((value, config) => config.withIgnoreVenvDir(value)),
      opt[Seq[String]]("ignore-paths")
        .text("Ignores the specified path from analysis. If not absolute it is interpreted relative to input-dir.")
        .action((value, config) => config.withIgnorePaths(value.map(Paths.get(_)))),
      opt[Seq[String]]("ignore-dir-names")
        .text(
          "Excludes all files where the relative path from input-dir contains at least one of names specified here."
        )
        .action((value, config) => config.withIgnoreDirNames(value)),
      XTypeRecoveryConfig.parserOptionsForParserConfig
    )
  }
}

object NewMain
    extends X2CpgMain(cmdLineParser, new Py2CpgOnFileSystem())(Py2CpgOnFileSystemConfig())
    with FrontendHTTPServer[Py2CpgOnFileSystemConfig, Py2CpgOnFileSystem] {

  override protected def newDefaultConfig(): Py2CpgOnFileSystemConfig = Py2CpgOnFileSystemConfig()

  def run(config: Py2CpgOnFileSystemConfig, frontend: Py2CpgOnFileSystem): Unit = {
    if (config.serverMode) { startup() }
    else { frontend.run(config) }
  }

  def getCmdLineParser = cmdLineParser

}
