package io.joern.ghidra2cpg

import io.joern.ghidra2cpg.Frontend._
import io.joern.x2cpg.{X2CpgConfig, X2CpgMain}
import scopt.OParser

/** Command line configuration parameters
  */
final case class Config(inputPath: String = "", outputPath: String = X2CpgConfig.defaultOutputPath)
    extends X2CpgConfig[Config] {

  override def withInputPath(inputPath: String): Config =
    copy(inputPath = inputPath)
  override def withOutputPath(x: String): Config = copy(outputPath = x)
}

private object Frontend {

  implicit val defaultConfig: Config = Config()

  val cmdLineParser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    import builder.programName
    OParser.sequence(programName("ghidra2cpg"))
  }
}

object Main extends X2CpgMain(cmdLineParser, new Ghidra2Cpg()) {
  def run(config: Config, ghidra2Cpg: Ghidra2Cpg): Unit = {
    ghidra2Cpg.run(config)
  }
}
