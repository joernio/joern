package io.joern.gosrc2cpg

import io.joern.gosrc2cpg.Frontend._
import io.joern.x2cpg.{X2CpgConfig, X2CpgMain}
import scopt.OParser

import java.nio.file.Paths

final case class Config() extends X2CpgConfig[Config] {}

object Frontend {
  implicit val defaultConfig: Config = Config()

  val cmdLineParser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    import builder._
    OParser.sequence(programName("gosrc2cpg"))
  }

}

object Main extends X2CpgMain(cmdLineParser, new GoSrc2Cpg()) {

  def run(config: Config, gosrc2cpg: GoSrc2Cpg): Unit = {
    val absPath = Paths.get(config.inputPath).toAbsolutePath.toString
    gosrc2cpg.run(config.withInputPath(absPath))
  }
}
