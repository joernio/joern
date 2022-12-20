package io.joern.pysrc2cpg

import io.joern.pysrc2cpg.Frontend.{cmdLineParser, defaultConfig}
import io.joern.pysrc2cpg.utils.Environment
import io.joern.x2cpg.{X2CpgConfig, X2CpgMain}
import scopt.OParser

import java.nio.file.Paths
import scala.util.matching.Regex

final case class Config(
                         inputPath: String = "",
                         outputPath: String = X2CpgConfig.defaultOutputPath,
                         ignoredFilesRegex: Regex = "".r,
                         ignoredFiles: Seq[String] = Seq.empty,
                         venvDir: String = ".venv",
                         ignoreVenvDir: Boolean = true
                       ) extends X2CpgConfig[Config] {

  def createPathForIgnore(ignore: String): String = {
    val path = Paths.get(ignore)
    if (path.isAbsolute) {
      path.toString
    } else {
      Paths.get(inputPath, ignore).toAbsolutePath.normalize().toString
    }
  }

  override def withInputPath(inputPath: String): Config = copy(inputPath = inputPath)
  override def withOutputPath(x: String): Config        = copy(outputPath = x)
}

private object Frontend {
  implicit val defaultConfig: Config = Config()

  val cmdLineParser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    import builder._
    OParser.sequence(
      programName("pysrc2cpg"),
      opt[Seq[String]]("exclude") // TODO maybe remove this from frontend. Also test this
        .valueName("<file1>,<file2>,...")
        .action((x, c) => c.copy(ignoredFiles = c.ignoredFiles ++ x.map(c.createPathForIgnore)))
        .text("files or folders to exclude during CPG generation (paths relative to <input-dir> or absolute paths)"),
      opt[String]("exclude-regex")
        .action((x, c) => c.copy(ignoredFilesRegex = x.r))
        .text("a regex specifying files to exclude during CPG generation (the absolute file path is matched)"),
      opt[Boolean]("ignore-venv-dir")
        .action((x, c) => c.copy(ignoreVenvDir = x))
        .text("specify whether to ignore venv-dir or not (defaults to true)"),
      opt[String]("venv-dir")
        .action((x, c) => c.copy(venvDir = x))
        .text("path of virtual environment directory to ignore (defaults to .venv)")
    )
  }

}

object Main extends X2CpgMain(cmdLineParser, new Py2Cpg()) {

  def run(config: Config, py2Cpg: Py2Cpg): Unit = {
    val absPath = Paths.get(config.inputPath).toAbsolutePath.toString
    var newIgnoredFiles = config.ignoredFiles
    if (config.ignoreVenvDir) {
      newIgnoredFiles = config.ignoredFiles ++ Seq(config.createPathForIgnore(config.venvDir))
    }
    if (Environment.pathExists(absPath)) {
      py2Cpg.run(config.copy(inputPath = absPath, ignoredFiles = newIgnoredFiles))
    } else {
      System.exit(1)
    }
  }

}