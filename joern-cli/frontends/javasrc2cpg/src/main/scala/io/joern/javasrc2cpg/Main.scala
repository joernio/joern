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
  delombokMode: Option[String] = None
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
      opt[String]("delombok-mode")
        .text("""Specifies how delombok should be executed. Options are
				 | no-delombok => to not run delombok under any circumstances.
                 | default => run delombok if a lombok dependency is found and analyse delomboked code.
                 | types-only => to run delombok, but use it for type information only
                 | run-delombok => to force run delombok and analyse delomboked code.""".stripMargin)
        .action((mode, c) => c.copy(delombokMode = Some(mode)))
    )
  }
}

object Main extends X2CpgMain(cmdLineParser, new JavaSrc2Cpg()) {
  def run(config: Config, javasrc2Cpg: JavaSrc2Cpg): Unit = {
    javasrc2Cpg.run(config)
  }
}
