package io.joern.x2cpg.utils.dependency

import io.joern.x2cpg.utils.dependency.GradleConfigKeys.GradleConfigKey

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
  private val defaultGradleProjectName       = "app"
  private val defaultGradleConfigurationName = "releaseCompileClasspath"

  def getDependencies(
    projectDir: Path,
    params: DependencyResolverParams = new DependencyResolverParams
  ): collection.Seq[String] = {
    if (isMavenBuild(projectDir)) {
      MavenDependencies.get(projectDir)
    } else if (isGradleBuild(projectDir)) {
      val gradleProjectName = params.forGradle.getOrElse(GradleConfigKeys.ProjectName, defaultGradleProjectName)
      val gradleConfiguration =
        params.forGradle.getOrElse(GradleConfigKeys.ConfigurationName, defaultGradleConfigurationName)
      GradleDependencies.get(projectDir, gradleProjectName, gradleConfiguration)
    } else {
      Nil
    }
  }

  def isMavenBuild(codeDir: Path): Boolean = {
    Files.exists(codeDir.resolve("pom.xml"))
  }

  def isGradleBuild(codeDir: Path): Boolean = {
    Files
      .walk(codeDir)
      .filter(file => file.toString.endsWith(".gradle") || file.toString.endsWith(".gradle.kts"))
      .count > 0
  }
}
