package io.joern.javasrc2cpg

import io.joern.javasrc2cpg.Frontend._
import io.joern.x2cpg.{X2CpgConfig, X2CpgMain}
import scopt.OParser

/** Command line configuration parameters
  */
final case class Config(
  inputPaths: Set[String] = Set.empty,
  outputPath: String = X2CpgConfig.defaultOutputPath,
  inferenceJarPaths: Set[String] = Set.empty
) extends X2CpgConfig[Config] {

  override def withAdditionalInputPath(inputPath: String): Config =
    copy(inputPaths = inputPaths + inputPath)
  override def withOutputPath(x: String): Config = copy(outputPath = x)
}

private object Frontend {

  implicit val defaultConfig: Config = Config()

  val cmdLineParser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    import builder._
    OParser.sequence(
      programName("javasrc2cpg"),
      opt[String]("inference-jar-paths")
        .text(s"extra jars used only for type information")
        .action((path, c) => c.copy(inferenceJarPaths = c.inferenceJarPaths + path))
    )
  }
}

object Main extends X2CpgMain(cmdLineParser, new JavaSrc2Cpg()) {
  def run(config: Config, javasrc2Cpg: JavaSrc2Cpg): Unit = {
    javasrc2Cpg.run(config)
  }
}
