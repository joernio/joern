package io.joern.x2cpg.utils.dependency

import io.joern.x2cpg.utils.dependency.GradleConfigKeys.GradleConfigKey
import org.slf4j.LoggerFactory

import java.nio.file.{Files, Path}

object GradleConfigKeys extends Enumeration {
  type GradleConfigKey = Value
  val ProjectName, ConfigurationName = Value
}
case class DependencyResolverParams(
  forMaven: Map[String, String] = Map(),
  forGradle: Map[GradleConfigKey, String] = Map()
)

object DependencyResolver {
  private val logger                         = LoggerFactory.getLogger(getClass)
  private val defaultGradleProjectName       = "app"
  private val defaultGradleConfigurationName = "releaseCompileClasspath"

  def getDependencies(
    projectDir: Path,
    params: DependencyResolverParams = new DependencyResolverParams
  ): Option[collection.Seq[String]] = {
    if (isMavenBuild(projectDir)) {
      Some(MavenDependencies.get(projectDir))
    } else if (isGradleBuild(projectDir)) {
      val gradleProjectName = params.forGradle.getOrElse(GradleConfigKeys.ProjectName, defaultGradleProjectName)
      val gradleConfiguration =
        params.forGradle.getOrElse(GradleConfigKeys.ConfigurationName, defaultGradleConfigurationName)
      GradleDependencies.get(projectDir, gradleProjectName, gradleConfiguration) match {
        case Some(deps) => Some(deps)
        case None =>
          logger.warn(s"Could not download Gradle dependencies for project at path `$projectDir`")
          None
      }
    } else {
      logger.warn(s"Could not find a supported build tool setup at path `$projectDir`")
      None
    }
  }

  def isMavenBuild(codeDir: Path): Boolean = {
    Files.exists(codeDir.resolve("pom.xml"))
  }

  def isGradleBuild(codeDir: Path): Boolean = {
    Files
      .walk(codeDir)
      .anyMatch(file => file.toString.endsWith(".gradle") || file.toString.endsWith(".gradle.kts"))
  }
}
