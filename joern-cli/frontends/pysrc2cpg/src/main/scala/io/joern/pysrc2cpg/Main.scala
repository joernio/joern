package io.joern.pysrc2cpg

import io.joern.pysrc2cpg.Frontend.cmdLineParser
import io.joern.x2cpg.X2CpgMain
import scopt.OParser

import java.nio.file.Paths

private object Frontend {
  val cmdLineParser: OParser[Unit, Py2CpgOnFileSystemConfig] = {
    val builder = OParser.builder[Py2CpgOnFileSystemConfig]
    import builder._
    OParser.sequence(
      programName("pysrc2cpg"),
      opt[String]("venvDir")
        // Default is specified in Py2CpgOFileSystemConfig because Scopt is a shit library.
        .text("Virtual environment directory. Defaults to .venv.")
        .action((dir, config) => config.withVenvDir(Paths.get(dir))),
      opt[Boolean]("ignoreVenvDir")
        // Default is specified in Py2CpgOFileSystemConfig because Scopt is a shit library.
        .text("Specifies whether venv-dir is ignored. Default to true.")
        .action(((value, config) => config.withIgnoreVenvDir(value))),
      opt[Unit]("no-dummyTypes")
        .hidden()
        .action((_, c) => c.withDisableDummyTypes(true))
        .text("disable generation of dummy types during type recovery")
    )
  }
}

object NewMain extends X2CpgMain(cmdLineParser, new Py2CpgOnFileSystem())(new Py2CpgOnFileSystemConfig()) {
  def run(config: Py2CpgOnFileSystemConfig, frontend: Py2CpgOnFileSystem): Unit = {
    frontend.run(config)
  }

  def getCmdLineParser = cmdLineParser

}
