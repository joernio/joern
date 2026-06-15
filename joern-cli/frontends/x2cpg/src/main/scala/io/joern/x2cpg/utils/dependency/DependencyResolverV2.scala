package io.joern.x2cpg.utils.dependency

import io.joern.x2cpg.utils.dependency.DependencyResolver.{
  findSupportedBuildFiles,
  isGradleBuildFile,
  isMavenBuildFile,
  logger
}
import org.slf4j.LoggerFactory

import java.nio.file.{Path, Paths}
import scala.util.Try

/** Shared entry point for the V2 dependency-fetching pipeline. Decides whether to invoke V2 (based on the per-frontend
  * Config flag OR the ENABLE_DEPENDENCY_FETCHER_V2 env var), resolves the graph, validates it, and exposes the
  * transitive source directories so each frontend can override its own source-file discovery.
  */
object DependencyResolverV2 {

  private val logger = LoggerFactory.getLogger(getClass)

  val EnableEnvVar: String = "ENABLE_DEPENDENCY_RESOLVER_V2"

  /** Outcome of a V2 dependency resolution. `sourceDirs` is the set of directories returned by the graph's transitive
    * walk, validated to be non-empty and to include at least one entry that is equal to or contained within the
    * original input path. `artifactJars` is the AAR-materialized jar set for the same walk.
    */
  type DependencyResolutionV2Result = (sourceDirs: Set[Path], artifactJars: Set[Path])

  def resolve(
    inputPath: String,
    flagFromConfig: Boolean,
    params: DependencyResolverParams = new DependencyResolverParams
  ): Option[DependencyResolutionV2Result] = {
    val projectDir = Paths.get(inputPath).toAbsolutePath

    getDependencyGraph(projectDir, params) match {
      case None =>
        logger.warn(s"V2 dependency fetcher: no supported build file found at $inputPath. Falling back.")
        None

      case Some(graph) if graph.nodes.isEmpty =>
        logger.warn(s"V2 dependency fetcher: graph is empty for $inputPath. Falling back.")
        None

      case Some(graph) =>
        val (artifacts, sources) = graph.transitiveDependenciesForProjectsInDir(projectDir)
        if (sources.isEmpty) {
          logger.warn(s"V2 dependency fetcher: no source directories resolved for $inputPath. Falling back.")
          None
        } else if (!sources.exists(isContainedIn(_, projectDir))) {
          logger.warn(
            s"V2 dependency fetcher: none of the ${sources.size} resolved source dirs are within $inputPath. " +
              "This usually means inputDir does not match any project's sourceSet. Falling back."
          )
          None
        } else {
          logger.info(
            s"V2 dependency fetcher: using ${sources.size} source directories and ${artifacts.size} artifacts " +
              s"for $inputPath."
          )
          Some((sources, artifacts))
        }
    }
  }

  /** V2 dependency resolution: returns a structured [[DependencyGraph]] for either Gradle or Maven projects, with one
    * node per project (Gradle) or a single `:root` node carrying all resolved artifacts (Maven). Returns None if no
    * supported build file is found.
    *
    * The graph models source dependencies (project-to-project edges) explicitly, which the V1 path collapsed into a
    * flat jar list. Callers wanting the legacy flat-list shape should call
    * `graph.transitiveDependenciesForProjectsInDir(projectDir)` to recover it.
    */
  def getDependencyGraph(
    projectDir: Path,
    params: DependencyResolverParams = new DependencyResolverParams
  ): Option[DependencyGraph] = {
    findSupportedBuildFiles(projectDir).headOption.flatMap { buildFile =>
      if (isGradleBuildFile(buildFile)) {
        val maybeConfigurationOverride = params.forGradle.get(GradleConfigKeys.ConfigurationName)
        val maybeAndroidVariant        = params.forGradle.get(GradleConfigKeys.AndroidVariant)
        Some(GradleDependenciesV2.get(buildFile.getParent, maybeConfigurationOverride, maybeAndroidVariant))
      } else if (isMavenBuildFile(buildFile)) {
        Some(MavenDependencies.getGraph(buildFile.getParent))
      } else {
        logger.warn(s"Found unsupported build file $buildFile")
        None
      }
    }
  }

  private def isContainedIn(candidate: Path, parent: Path): Boolean = {
    // Resolve symlinks before comparing — gradle reports real paths but the input may have
    // arrived through a symlinked tree (common in test setups).
    def realPath(p: Path): Path = Try(p.toRealPath()).getOrElse(p.toAbsolutePath.normalize())
    realPath(candidate).startsWith(realPath(parent))
  }
}
