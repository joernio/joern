package io.joern.kotlin2cpg

import io.joern.kotlin2cpg.Frontend.*
import io.joern.x2cpg.{DependencyDownloadConfig, X2CpgConfig, X2CpgMain}
import io.joern.x2cpg.utils.server.FrontendHTTPServer
import scopt.OParser

case class DefaultContentRootJarPath(path: String, isResource: Boolean)

final case class Config(
  classpath: Set[String] = Set.empty,
  withStdlibJarsInClassPath: Boolean = true,
  gradleProjectName: Option[String] = None,
  gradleConfigurationName: Option[String] = None,
  jar4importServiceUrl: Option[String] = None,
  includeJavaSourceFiles: Boolean = false,
  generateNodesForDependencies: Boolean = false,
  downloadDependencies: Boolean = false,
  keepTypeArguments: Boolean = false,
  override val genericConfig: X2CpgConfig.GenericConfig = X2CpgConfig.GenericConfig()
) extends X2CpgConfig[Config]
    with DependencyDownloadConfig {

  override def withGenericConfig(value: X2CpgConfig.GenericConfig): Config =
    copy(genericConfig = value)

  def withClasspath(classpath: Set[String]): Config = {
    this.copy(classpath = classpath)
  }

  def withStdLibJars(value: Boolean): Config = {
    this.copy(withStdlibJarsInClassPath = value)
  }

  def withGradleProjectName(name: String): Config = {
    this.copy(gradleProjectName = Some(name))
  }

  def withGradleConfigurationName(name: String): Config = {
    this.copy(gradleConfigurationName = Some(name))
  }

  def withJar4ImportServiceUrl(url: String): Config = {
    this.copy(jar4importServiceUrl = Some(url))
  }

  def withIncludeJavaSourceFiles(value: Boolean): Config = {
    this.copy(includeJavaSourceFiles = value)
  }

  def withGenerateNodesForDependencies(value: Boolean): Config = {
    this.copy(generateNodesForDependencies = value)
  }

  override def withDownloadDependencies(value: Boolean): Config = {
    this.copy(downloadDependencies = value)
  }

  def withKeepTypeArguments(value: Boolean): Config = {
    copy(keepTypeArguments = value)
  }
}

private object Frontend {
  implicit val defaultConfig: Config = Config()

  val cmdLineParser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    import builder.programName
    import builder.opt
    OParser.sequence(
      programName("kotlin2cpg"),
      opt[String]("classpath")
        .unbounded()
        .text("Add entry to classpath")
        .action((incl, c) => c.withClasspath(c.classpath + incl)),
      opt[Unit]("no-stdlib-jars")
        .text("Do not add local versions of Kotlin stdlib jars to classpath")
        .action((_, c) => c.withStdLibJars(false)),
      opt[String]("jar4import-url")
        .text("Set URL of service which fetches necessary dependency jars for import names found in the project")
        .action((value, c) => c.withJar4ImportServiceUrl(value)),
      opt[String]("gradle-project-name")
        .text("Name of the Gradle project used to download dependencies")
        .action((value, c) => c.withGradleProjectName(value)),
      opt[String]("gradle-configuration-name")
        .text("Name of the Gradle configuration used to download dependencies")
        .action((value, c) => c.withGradleConfigurationName(value)),
      opt[Unit]("include-java-sources")
        .text("Include Java sources in the resulting CPG")
        .action((_, c) => c.withIncludeJavaSourceFiles(true)),
      opt[Unit]("generate-nodes-for-dependencies")
        .text("Generate nodes for the dependencies of the target project")
        .action((_, c) => c.withGenerateNodesForDependencies(true)),
      DependencyDownloadConfig.parserOptions,
      opt[Unit]("keep-type-arguments")
        .hidden()
        .action((_, c) => c.withKeepTypeArguments(true))
        .text("Type full names of variables keep their type arguments. (Deprecated, no effect.")
    )
  }
}

object Main extends X2CpgMain(new Kotlin2Cpg(), cmdLineParser.asInstanceOf)
