package io.joern.x2cpg.utils.dependency

import io.joern.x2cpg.utils.dependency.DependencyResolver.{findSupportedBuildFiles, isGradleBuildFile, isMavenBuildFile}
import io.shiftleft.semanticcpg.utils.FileUtil
import org.slf4j.LoggerFactory

import java.nio.file.{Files, Path, Paths}
import scala.util.{Failure, Success, Try}

/** Shared entry point for the V2 dependency-fetching pipeline. Decides whether to invoke V2 (based on the per-frontend
  * Config flag OR the [[EnableEnvVar]] env var), resolves the graph, validates it, and exposes the transitive source
  * directories so each frontend can override its own source-file discovery.
  */
object DependencyResolverV2 {

  private val logger = LoggerFactory.getLogger(getClass)

  val EnableEnvVar: String = "ENABLE_DEPENDENCY_RESOLVER_V2"

  /** Returns true when V2 should be attempted — either the per-frontend CLI flag is set, or [[EnableEnvVar]] is present
    * in the environment. Logs the decision (CLI flag / env var / neither) at info so each frontend doesn't have to
    * duplicate the same three log lines.
    */
  def isRequested(cliFlag: Boolean): Boolean = {
    val envVarSet = sys.env.contains(EnableEnvVar)
    if (cliFlag) {
      logger.info("Enabling dependency resolver v2 as CLI flag is set")
    } else if (envVarSet) {
      logger.info(s"Enabling dependency resolver v2 as $EnableEnvVar env var is set")
    } else {
      logger.info(
        "Using dependency resolver v1 (if dependency resolution is enabled) as neither the CLI flag nor env var enabling v2 are set"
      )
    }
    cliFlag || envVarSet
  }

  /** Env-var override for the Android build variant used during Gradle resolution. V2 intentionally does not honour
    * [[GradleConfigKeys.ProjectName]] (it resolves every reachable project), but the Android variant has no CLI flag,
    * so we plumb it through this env var. When set, it is forwarded as [[GradleConfigKeys.AndroidVariant]] in the
    * resolver params.
    */
  val AndroidVariantEnvVar: String = "SL_ANDROID_VARIANT"

  /** Outcome of a V2 dependency resolution. `sourceDirs` is the set of directories returned by the graph's transitive
    * walk, validated to be non-empty. `artifactJars` is the AAR-materialized jar set for the same walk.
    */
  type DependencyResolutionResult = (sourceDirs: Set[Path], artifactJars: Set[Path])

  def resolve(
    inputPath: String,
    params: DependencyResolverParams = new DependencyResolverParams
  ): Option[DependencyResolutionResult] = {
    val projectDir = Paths.get(inputPath).toAbsolutePath

    getDependencyGraph(projectDir, params) match {
      case None =>
        logger.warn(s"V2 dependency fetcher: no supported build file found at $inputPath. Falling back.")
        None

      case Some(graph) if graph.nodes.isEmpty =>
        logger.warn(s"V2 dependency fetcher: graph is empty for $inputPath. Falling back.")
        None

      case Some(graph) =>
        val result = graph.transitiveDependenciesForProjectsInDir(projectDir)
        if (result.sourceDirs.isEmpty) {
          logger.warn(s"V2 dependency fetcher: no source directories resolved for $inputPath. Falling back.")
          None
        } else {
          logger.info(
            s"V2 dependency fetcher: using ${result.sourceDirs.size} source directories and " +
              s"${result.artifactJars.size} artifacts for $inputPath."
          )
          Some(result)
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
    val buildFiles                     = findSupportedBuildFiles(projectDir)
    val (gradleBuildFiles, otherFiles) = buildFiles.partition(isGradleBuildFile)
    val mavenBuildFiles                = otherFiles.filter(isMavenBuildFile)
    otherFiles.filterNot(isMavenBuildFile).foreach { unsupported =>
      logger.warn(s"Found unsupported build file $unsupported")
    }

    if (gradleBuildFiles.nonEmpty) {
      if (mavenBuildFiles.nonEmpty) {
        logger.warn(
          s"V2 dependency fetcher: ${mavenBuildFiles.size} Maven build file(s) coexist with Gradle peers; skipping Maven."
        )
      }
      val maybeConfigurationOverride = params.forGradle.get(GradleConfigKeys.ConfigurationName)
      val maybeAndroidVariant        = params.forGradle.get(GradleConfigKeys.AndroidVariant)
      Try(Files.createTempDirectory(GradleDependenciesV2.tempDirPrefix)) match {
        case Failure(ex) =>
          logger.warn(s"Could not create temporary directory for saving dependency files: ${ex.getMessage}")
          logger.debug("Full exception: ", ex)
          None
        case Success(destinationDir) =>
          FileUtil.deleteOnExit(destinationDir)
          val dependencyInfoDir = destinationDir.resolve(GradleDependenciesV2.dependencyInfoSubdir)
          gradleBuildFiles.foreach { buildFile =>
            GradleDependenciesV2.get(
              buildFile.getParent,
              destinationDir,
              maybeConfigurationOverride,
              maybeAndroidVariant
            )
          }
          val jsonPaths = GradleDependenciesV2.listDependencyInfoFiles(dependencyInfoDir)
          logger.info(s"Loaded ${jsonPaths.length} project dependency info files from $dependencyInfoDir.")
          Some(DependencyGraph.fromJson(destinationDir, jsonPaths))
      }
    } else {
      mavenBuildFiles.headOption.map(buildFile => MavenDependencies.getGraph(buildFile.getParent))
    }
  }
}
