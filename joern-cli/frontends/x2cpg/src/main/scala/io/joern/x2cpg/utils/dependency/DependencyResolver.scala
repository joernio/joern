package io.joern.x2cpg.utils.dependency

import io.joern.x2cpg.utils.dependency.GradleConfigKeys.GradleConfigKey
import org.slf4j.LoggerFactory

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._

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
      logger.info("resolving Maven dependencies at {}", projectDir)
      Some(MavenDependencies.get(projectDir))
    } else {
      getGradleProjects(projectDir) match {
        case Nil => // Not a Gradle project
          logger.warn(s"Could not find a supported build tool setup at path `$projectDir`")
          None

        case gradleProjectDirs =>
          val combinedDependencies = gradleProjectDirs.flatMap(getDepsForGradleProject(params, _)).flatten
          Option.when(combinedDependencies.nonEmpty) { combinedDependencies }
      }
    }
  }

  private def getDepsForGradleProject(
    params: DependencyResolverParams,
    projectDir: Path
  ): Option[collection.Seq[String]] = {
    logger.info("resolving Gradle dependencies at {}", projectDir)
    val gradleProjectName = params.forGradle.getOrElse(GradleConfigKeys.ProjectName, defaultGradleProjectName)
    val gradleConfiguration =
      params.forGradle.getOrElse(GradleConfigKeys.ConfigurationName, defaultGradleConfigurationName)
    GradleDependencies.get(projectDir, gradleProjectName, gradleConfiguration) match {
      case Some(deps) => Some(deps)
      case None =>
        logger.warn(s"Could not download Gradle dependencies for project at path `$projectDir`")
        None
    }
  }

  def isMavenBuild(codeDir: Path): Boolean = {
    Files
      .walk(codeDir)
      .anyMatch(file => file.toString.endsWith("pom.xml"))
  }

  def getGradleProjects(codeDir: Path): List[Path] = {
    Files
      .walk(codeDir)
      .filter(isGradlePath)
      .map(_.getParent)
      .iterator()
      .asScala
      .toList
  }

  private def isGradlePath(path: Path): Boolean = {
    val pathString = path.toString
    pathString.endsWith(".gradle") || pathString.endsWith(".gradle.kts")
  }
}
