package io.joern.javasrc2cpg

import io.joern.javasrc2cpg.Frontend.*
import io.joern.x2cpg.passes.frontend.{TypeRecoveryParserConfig, XTypeRecovery}
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
  jdkPath: Option[String] = None,
  showEnv: Boolean = false
) extends X2CpgConfig[Config]
    with TypeRecoveryParserConfig[Config] {
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

  def withJdkPath(path: String): Config = {
    copy(jdkPath = Some(path)).withInheritedFields(this)
  }

  def withShowEnv(value: Boolean): Config = {
    copy(showEnv = value).withInheritedFields(this)
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
      XTypeRecovery.parserOptions,
      opt[String]("jdk-path")
        .action((path, c) => c.withJdkPath(path))
        .text("JDK used for resolving builtin Java types. If not set, current classpath will be used"),
      opt[Unit]("show-env")
        .action((_, c) => c.withShowEnv(true))
        .text("print information about environment variables used by javasrc2cpg and exit.")
    )
  }
}

object Main extends X2CpgMain(cmdLineParser, new JavaSrc2Cpg()) {

  override def main(args: Array[String]): Unit = {
    // TODO: This is a hack to allow users to use the "--show-env" option without having
    //  to specify an input argument. Clean this up when adding this option to more frontends.
    if (args.contains("--show-env")) {
      super.main(Array("--show-env", "<input_dir_placeholder>"))
    } else {
      super.main(args)
    }
  }

  def run(config: Config, javasrc2Cpg: JavaSrc2Cpg): Unit = {
    if (config.showEnv) {
      JavaSrc2Cpg.showEnv()
    } else {
      javasrc2Cpg.run(config)
    }
  }

  def getCmdLineParser = cmdLineParser
}
