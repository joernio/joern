package io.joern.kotlin2cpg

import io.joern.kotlin2cpg.Frontend._
import io.joern.x2cpg.{X2CpgConfig, X2CpgMain}
import scopt.OParser

case class DefaultContentRootJarPath(path: String, isResource: Boolean)

final case class Config(
  classpath: Set[String] = Set.empty,
  withStdlibJarsInClassPath: Boolean = true,
  downloadDependencies: Boolean = false,
  gradleProjectName: Option[String] = None,
  gradleConfigurationName: Option[String] = None,
  jar4importServiceUrl: Option[String] = None,
  includeJavaSourceFiles: Boolean = false,
  generateNodesForDependencies: Boolean = false
) extends X2CpgConfig[Config] {

  def withClasspath(classpath: Set[String]): Config = {
    this.copy(classpath = classpath).withInheritedFields(this)
  }

  def withStdLibJars(value: Boolean): Config = {
    this.copy(withStdlibJarsInClassPath = value).withInheritedFields(this)
  }

  def withDownloadDependencies(value: Boolean): Config = {
    this.copy(downloadDependencies = value).withInheritedFields(this)
  }

  def withGradleProjectName(name: String): Config = {
    this.copy(gradleProjectName = Some(name)).withInheritedFields(this)
  }

  def withGradleConfigurationName(name: String): Config = {
    this.copy(gradleConfigurationName = Some(name)).withInheritedFields(this)
  }

  def withJar4ImportServiceUrl(url: String): Config = {
    this.copy(jar4importServiceUrl = Some(url)).withInheritedFields(this)
  }

  def withIncludeJavaSourceFiles(value: Boolean): Config = {
    this.copy(includeJavaSourceFiles = value).withInheritedFields(this)
  }

  def withGenerateNodesForDependencies(value: Boolean): Config = {
    this.copy(generateNodesForDependencies = value).withInheritedFields(this)
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
      opt[Unit]("download-dependencies")
        .text("Download the dependencies of the target project and add them to the classpath")
        .action((_, c) => c.withDownloadDependencies(true)),
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
        .action((_, c) => c.withGenerateNodesForDependencies(true))
    )
  }
}

object Main extends X2CpgMain(cmdLineParser, new Kotlin2Cpg()) {
  def run(config: Config, kotlin2cpg: Kotlin2Cpg): Unit = {
    kotlin2cpg.run(config)
  }
}
