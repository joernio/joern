package io.joern.javasrc2cpg

import io.joern.javasrc2cpg.Frontend._
import io.joern.x2cpg.{X2CpgConfig, X2CpgMain}
import scopt.OParser

/** Command line configuration parameters
  */
final case class Config(
  inputPath: String = "",
  outputPath: String = X2CpgConfig.defaultOutputPath,
  inferenceJarPaths: Set[String] = Set.empty,
  fetchDependencies: Boolean = false,
  javaFeatureSetVersion: Option[String] = None,
  delombokJavaHome: Option[String] = None,
  runDelombok: Boolean = false
) extends X2CpgConfig[Config] {

  override def withInputPath(inputPath: String): Config =
    copy(inputPath = inputPath)
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
        .action((path, c) => c.copy(inferenceJarPaths = c.inferenceJarPaths + path)),
      opt[Unit]("fetch-dependencies")
        .text("attempt to fetch dependencies jars for extra type information")
        .action((_, c) => c.copy(fetchDependencies = true)),
      opt[String]("delombok-java-home")
        .text("Optional override to set java home used to run Delombok. Java 17 is recommended for the best results.")
        .action((path, c) => c.copy(delombokJavaHome = Some(path))),
      opt[Unit]("run-delombok")
        .text("run delombok on source before scanning for more accurate methods and type results")
        .action((_, c) => c.copy(runDelombok = true))
    )
  }
}

object Main extends X2CpgMain(cmdLineParser, new JavaSrc2Cpg()) {
  def run(config: Config, javasrc2Cpg: JavaSrc2Cpg): Unit = {
    javasrc2Cpg.run(config)
  }
}
