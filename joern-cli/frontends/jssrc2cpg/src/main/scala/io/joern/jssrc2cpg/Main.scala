package io.joern.jssrc2cpg

import io.joern.jssrc2cpg.Frontend._
import io.joern.jssrc2cpg.utils.Environment
import io.joern.x2cpg.{X2CpgConfig, X2CpgMain}
import scopt.OParser

import java.nio.file.Paths

final case class Config(tsTypes: Boolean = true, disableDummyTypes: Boolean = false) extends X2CpgConfig[Config] {
  def withTsTypes(value: Boolean): Config = {
    copy(tsTypes = value).withInheritedFields(this)
  }

  def withDisableDummyTypes(value: Boolean): Config = {
    copy(disableDummyTypes = value).withInheritedFields(this)
  }
}

object Frontend {
  implicit val defaultConfig: Config = Config()

  val cmdLineParser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    import builder._
    OParser.sequence(
      programName("jssrc2cpg"),
      opt[Unit]("no-tsTypes")
        .hidden()
        .action((_, c) => c.withTsTypes(false))
        .text("disable generation of types via Typescript"),
      opt[Unit]("no-dummyTypes")
        .hidden()
        .action((_, c) => c.withDisableDummyTypes(true))
        .text("disable generation of dummy types during type recovery")
    )
  }

}

object Main extends X2CpgMain(cmdLineParser, new JsSrc2Cpg()) {

  def run(config: Config, jssrc2cpg: JsSrc2Cpg): Unit = {
    val absPath = Paths.get(config.inputPath).toAbsolutePath.toString
    if (Environment.pathExists(absPath)) {
      jssrc2cpg.run(config.withInputPath(absPath))
    } else {
      System.exit(1)
    }
  }

}
