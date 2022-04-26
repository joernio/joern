package io.joern.x2cpg.utils.dependency

import java.nio.file.Path

object DependencyResolver {
  def getDependencies(projectDir: Path): collection.Seq[String] = {
    if (MavenDependencies.isMavenBuild(projectDir)) {
      MavenDependencies.get(projectDir)
    } else if (GradleDependencies.isGradleBuild(projectDir)) {
      GradleDependencies.downloadRuntimeLibs(projectDir)
    } else {
      Nil
    }
  }
}
