package io.joern.x2cpg.utils.dependency

import java.nio.file.{Files, Path}

object DependencyResolver {
  def getDependencies(projectDir: Path): collection.Seq[String] = {
    if (isMavenBuild(projectDir)) {
      MavenDependencies.get(projectDir)
    } else if (isGradleBuild(projectDir)) {
      GradleDependencies.get(projectDir)
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
