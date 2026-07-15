package io.joern.x2cpg.utils.dependency

import io.joern.x2cpg.SourceFiles
import io.shiftleft.semanticcpg.utils.ExternalCommand
import org.slf4j.LoggerFactory

import java.nio.file.Path
import scala.util.{Failure, Success}

enum GradleConfigKeys {
  case ProjectName, ConfigurationName, AndroidVariant
}

case class DependencyResolverParams(
  forMaven: Map[String, String] = Map(),
  forGradle: Map[GradleConfigKeys, String] = Map()
)

object DependencyResolver {
  private val logger                               = LoggerFactory.getLogger(getClass)
  private val GradleBuildFileSuffixes: Set[String] = Set(".gradle", ".gradle.kts")
  private val MavenBuildFileSuffixes: Set[String]  = Set("pom.xml")
  private val BuildFileSuffixes: Set[String]       = GradleBuildFileSuffixes ++ MavenBuildFileSuffixes

  def getCoordinates(
    projectDir: Path,
    params: DependencyResolverParams = new DependencyResolverParams
  ): Option[collection.Seq[String]] = {
    val coordinates = findSupportedBuildFiles(projectDir).flatMap { buildFile =>
      if (isMavenBuildFile(buildFile))
        // TODO: implement
        None
      else if (isGradleBuildFile(buildFile)) {
        // TODO: Don't limit this to the default configuration name
        getCoordinatesForGradleProject(buildFile.getParent, "compileClasspath")
      } else {
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
    val lines = ExternalCommand
      .run(
        command = Seq("gradle", "dependencies", "--configuration,", configuration),
        workingDir = Option(projectDir),
        additionalContext = "trying to retrieve dependencies for Gradle project at path `$projectDir`"
      )
      .logIfFailed()
      .successOption
      .getOrElse(Seq.empty)

    val coordinates = MavenCoordinates.fromGradleOutput(lines)
    logger.info("Got {} Maven coordinates", coordinates.size)
    Some(coordinates)
  }

  def getDependencies(
    projectDir: Path,
    params: DependencyResolverParams = new DependencyResolverParams
  ): Option[Seq[String]] = {
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

    Option.when(dependencies.nonEmpty) {
      logger.debug(("Dependency jars fetched:" :: dependencies).mkString(s"${System.lineSeparator()} - "))
      dependencies
    }
  }

  private def getDepsForGradleProject(
    params: DependencyResolverParams,
    projectDir: Path
  ): Option[collection.Seq[String]] = {
    logger.info("resolving Gradle dependencies at {}", projectDir)
    val maybeProjectNameOverride   = params.forGradle.get(GradleConfigKeys.ProjectName)
    val maybeConfigurationOverride = params.forGradle.get(GradleConfigKeys.ConfigurationName)
    GradleDependencies.get(projectDir, maybeProjectNameOverride, maybeConfigurationOverride) match {
      case dependencies if dependencies.exists(_._2.nonEmpty) =>
        val allDependencies = dependencies.flatMap(_._2)
        Option(allDependencies.distinctBy(path => new java.io.File(path).getName))
      case _ =>
        logger.warn(s"Could not download Gradle dependencies for project at path `$projectDir`")
        None
    }
  }

  private[dependency] def isGradleBuildFile(file: Path): Boolean =
    GradleBuildFileSuffixes.exists(file.toString.endsWith)

  private[dependency] def isMavenBuildFile(file: Path): Boolean =
    MavenBuildFileSuffixes.exists(file.toString.endsWith)

  private[dependency] def findSupportedBuildFiles(currentDir: Path): List[Path] = {
    val allBuildFiles = SourceFiles.determine(
      currentDir,
      BuildFileSuffixes,
      ignoredDefaultRegex = Some(SourceFiles.JvmDefaultIgnoredFolders)
    )

    // Only fetch dependencies once for projects with both a build.gradle and a pom.xml file
    // by grouping per parent directory and preferring Gradle over Maven.
    val perDirectory = allBuildFiles
      .groupBy(_.getParent)
      .values
      .flatMap { filesInDir =>
        filesInDir
          .find(isGradleBuildFile)
          .orElse(filesInDir.find(isMavenBuildFile))
      }
      .toList

    // Keep only the top-most build file on each directory branch. After sorting by path,
    // any nested build file appears immediately after its ancestor's build file, so we can
    // drop it by checking a prefix against the last kept directory.
    perDirectory
      .sortBy(_.toString)
      .foldLeft(List.empty[Path]) { (kept, buildFile) =>
        kept.headOption match {
          case Some(top) if buildFile.getParent.startsWith(top.getParent) => kept
          case _                                                          => buildFile :: kept
        }
      }
      .reverse
  }
}
