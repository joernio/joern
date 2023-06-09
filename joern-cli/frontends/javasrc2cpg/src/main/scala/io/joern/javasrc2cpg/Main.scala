package io.joern.javasrc2cpg

import io.joern.javasrc2cpg.Frontend._
import io.joern.x2cpg.{X2CpgConfig, X2CpgMain}
import scopt.OParser

/** Command line configuration parameters
  */
final case class Config(
  inferenceJarPaths: Set[String] = Set.empty,
  fetchDependencies: Boolean = false,
  javaFeatureSetVersion: Option[String] = None,
  delombokJavaHome: Option[String] = None,
  delombokMode: Option[String] = None,
  enableTypeRecovery: Boolean = false,
  disableDummyTypes: Boolean = false
) extends X2CpgConfig[Config] {
  def withInferenceJarPaths(paths: Set[String]): Config = {
    copy(inferenceJarPaths = paths).withInheritedFields(this)
  }

  def withFetchDependencies(value: Boolean): Config = {
    copy(fetchDependencies = value).withInheritedFields(this)
  }

  def withJavaFeatureSetVersion(version: String): Config = {
    copy(javaFeatureSetVersion = Some(version)).withInheritedFields(this)
  }

  def withDelombokJavaHome(path: String): Config = {
    copy(delombokJavaHome = Some(path)).withInheritedFields(this)
  }

  def withDelombokMode(mode: String): Config = {
    copy(delombokMode = Some(mode)).withInheritedFields(this)
  }

  def withEnableTypeRecovery(value: Boolean): Config = {
    copy(enableTypeRecovery = value).withInheritedFields(this)
  }

  def withDisableDummyTypes(value: Boolean): Config = {
    copy(disableDummyTypes = value).withInheritedFields(this)
  }
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
        .action((path, c) => c.withInferenceJarPaths(c.inferenceJarPaths + path)),
      opt[Unit]("fetch-dependencies")
        .text("attempt to fetch dependencies jars for extra type information")
        .action((_, c) => c.withFetchDependencies(true)),
      opt[String]("delombok-java-home")
        .text("Optional override to set java home used to run Delombok. Java 17 is recommended for the best results.")
        .action((path, c) => c.withDelombokJavaHome(path)),
      opt[String]("delombok-mode")
        .text("""Specifies how delombok should be executed. Options are
				| no-delombok => to not run delombok under any circumstances.
                 | default => run delombok if a lombok dependency is found and analyse delomboked code.
                 | types-only => to run delombok, but use it for type information only
                 | run-delombok => to force run delombok and analyse delomboked code.""".stripMargin)
        .action((mode, c) => c.withDelombokMode(mode)),
      opt[Unit]("enable-type-recovery")
        .hidden()
        .action((_, c) => c.withEnableTypeRecovery(true))
        .text("enable generic type recovery"),
      opt[Unit]("no-dummyTypes")
        .hidden()
        .action((_, c) => c.withDisableDummyTypes(true))
        .text("disable generation of dummy types during type recovery")
    )
  }
}

object Main extends X2CpgMain(cmdLineParser, new JavaSrc2Cpg()) {
  def run(config: Config, javasrc2Cpg: JavaSrc2Cpg): Unit = {
    javasrc2Cpg.run(config)
  }

  def getCmdLineParser = cmdLineParser
}
