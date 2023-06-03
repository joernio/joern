package io.joern.jssrc2cpg

import io.joern.jssrc2cpg.Frontend._
import io.joern.jssrc2cpg.utils.Environment
import io.joern.x2cpg.{X2CpgConfig, X2CpgMain}
import scopt.OParser

import java.nio.file.Paths
import scala.util.matching.Regex

final case class Config(
  inputPath: String = "",
  outputPath: String = X2CpgConfig.defaultOutputPath,
  ignoredFilesRegex: Regex = "".r,
  ignoredFiles: Seq[String] = Seq.empty,
  tsTypes: Boolean = true,
  disableDummyTypes: Boolean = false
) extends X2CpgConfig[Config] {

  def createPathForIgnore(ignore: String): String = {
    val path = Paths.get(ignore)
    if (path.isAbsolute) {
      path.toString
    } else {
      Paths.get(inputPath, ignore).toAbsolutePath.normalize().toString
    }
  }

  override def withInputPath(inputPath: String): Config =
    copy(inputPath = Paths.get(inputPath).toAbsolutePath.normalize().toString)
  override def withOutputPath(x: String): Config = copy(outputPath = x)
}

object Frontend {
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
        .text("a regex specifying files to exclude during CPG generation (the absolute file path is matched)"),
      opt[Unit]("no-tsTypes")
        .hidden()
        .action((_, c) => c.copy(tsTypes = false))
        .text("disable generation of types via Typescript"),
      opt[Unit]("no-dummyTypes")
        .hidden()
        .action((_, c) => c.copy(disableDummyTypes = true))
        .text("disable generation of dummy types during type recovery")
    )
  }

}

object Main extends X2CpgMain(cmdLineParser, new JsSrc2Cpg()) {

  def run(config: Config, jssrc2cpg: JsSrc2Cpg): Unit = {
    val absPath = Paths.get(config.inputPath).toAbsolutePath.toString
    if (Environment.pathExists(absPath)) {
      jssrc2cpg.run(config.copy(inputPath = absPath))
    } else {
      System.exit(1)
    }
  }

}
