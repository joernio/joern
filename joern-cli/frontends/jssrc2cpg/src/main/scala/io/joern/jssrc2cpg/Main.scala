package io.joern.jssrc2cpg

import io.joern.jssrc2cpg.Frontend._
import io.joern.jssrc2cpg.utils.Environment
import io.joern.x2cpg.X2CpgConfig
import io.joern.x2cpg.X2CpgMain
import scopt.OParser

import java.nio.file.Paths
import scala.util.matching.Regex

final case class Config(
  inputPath: String = "",
  outputPath: String = X2CpgConfig.defaultOutputPath,
  ignoredFilesRegex: Regex = "".r,
  ignoredFiles: Seq[String] = Seq.empty
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
      programName("jssrc2cpg"),
      opt[Seq[String]]("exclude")
        .valueName("<file1>,<file2>,...")
        .action((x, c) => c.copy(ignoredFiles = c.ignoredFiles ++ x.map(c.createPathForIgnore)))
        .text("files or folders to exclude during CPG generation (paths relative to <input-dir> or absolute paths)"),
      opt[String]("exclude-regex")
        .action((x, c) => c.copy(ignoredFilesRegex = x.r))
        .text("a regex specifying files to exclude during CPG generation (the absolute file path is matched)")
    )
  }

}

object Main extends X2CpgMain(cmdLineParser, new JsSrc2Cpg()) {

  def run(config: Config, jssrc2cpg: JsSrc2Cpg): Unit = {
    if (Environment.pathExists(config.inputPath) && Environment.valid()) {
      jssrc2cpg.run(config)
    } else {
      System.exit(1)
    }
  }

}
