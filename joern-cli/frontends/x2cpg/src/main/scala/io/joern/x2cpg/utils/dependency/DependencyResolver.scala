package io.joern.x2cpg.utils.dependency

import better.files.File
import io.joern.x2cpg.utils.ExternalCommand
import io.joern.x2cpg.utils.dependency.GradleConfigKeys.GradleConfigKey
import org.slf4j.LoggerFactory

import java.nio.file.Path
import scala.util.{Failure, Success}

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
  private val defaultGradleConfigurationName = "compileClasspath"
  private val MaxSearchDepth: Int            = 4

  def getCoordinates(
    projectDir: Path,
    params: DependencyResolverParams = new DependencyResolverParams
  ): Option[collection.Seq[String]] = {
    val coordinates = findSupportedBuildFiles(projectDir).flatMap { buildFile =>
      if (isMavenBuildFile(buildFile))
        // TODO: implement
        None
      else if (isGradleBuildFile(buildFile))
        getCoordinatesForGradleProject(buildFile.getParent, defaultGradleConfigurationName)
      else {
        logger.warn(s"Found unsupported build file $buildFile")
        Nil
      }
    }.flatten

    Option.when(coordinates.nonEmpty)(coordinates)
  }

  private def getCoordinatesForGradleProject(
    projectDir: Path,
    configuration: String
  ): Option[collection.Seq[String]] = {
    val lines = ExternalCommand.run(s"gradle dependencies --configuration $configuration", projectDir.toString) match {
      case Success(lines) => lines
      case Failure(exception) =>
        logger.warn(
          s"Could not retrieve dependencies for Gradle project at path `$projectDir`\n" +
            exception.getMessage
        )
        Seq()
    }

    val coordinates = MavenCoordinates.fromGradleOutput(lines)
    logger.info("Got {} Maven coordinates", coordinates.size)
    Some(coordinates)
  }

  def getDependencies(
    projectDir: Path,
    params: DependencyResolverParams = new DependencyResolverParams
  ): Option[collection.Seq[String]] = {
    val dependencies = findSupportedBuildFiles(projectDir).flatMap { buildFile =>
      if (isMavenBuildFile(buildFile)) {
        MavenDependencies.get(buildFile.getParent)
      } else if (isGradleBuildFile(buildFile)) {
        getDepsForGradleProject(params, buildFile.getParent)
      } else {
        logger.warn(s"Found unsupported build file $buildFile")
        Nil
      }
    }.flatten

    Option.when(dependencies.nonEmpty)(dependencies)
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

  private def isGradleBuildFile(file: File): Boolean = {
    val pathString = file.pathAsString
    pathString.endsWith(".gradle") || pathString.endsWith(".gradle.kts")
  }

  private def isMavenBuildFile(file: File): Boolean = {
    file.pathAsString.endsWith("pom.xml")
  }

  private def findSupportedBuildFiles(currentDir: File, depth: Int = 0): List[Path] = {
    if (depth >= MaxSearchDepth) {
      logger.info("findSupportedBuildFiles reached max depth before finding build files")
      Nil
    } else {
      val (childDirectories, childFiles) = currentDir.children.partition(_.isDirectory)
      // Only fetch dependencies once for projects with both a build.gradle and a pom.xml file
      val childFileList = childFiles.toList
      childFileList
        .find(isGradleBuildFile)
        .orElse(childFileList.find(isMavenBuildFile)) match {
        case Some(buildFile) => buildFile.path :: Nil

        case None if childDirectories.isEmpty => Nil

        case None =>
          childDirectories.flatMap { dir =>
            findSupportedBuildFiles(dir, depth + 1)
          }.toList
      }
    }
  }
}
