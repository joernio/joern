package io.joern.x2cpg.utils.dependency

import io.joern.x2cpg.utils.dependency.DependencyGraph.logger
import org.apache.maven.artifact.versioning.ComparableVersion
import org.slf4j.LoggerFactory

import java.nio.file.Path
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

private[dependency] type ProjectName = String

/** Used for dependencies with maven-style coordinates (including from Gradle builds). This will need to be generalised
  * if the DependencyGraph is ever used for other languages.
  */
case class MavenArtifactDependency(
  path: Path,
  group: Option[String],
  name: String,
  version: Option[String],
  classifier: Option[String],
  extension: String
) {

  /** Identity excluding version — used to dedupe the same artifact pulled in at different versions by different
    * subprojects.
    */
  private[dependency] def identity: DependencyGraph.ArtifactIdentity =
    (group, name, classifier, extension)
}

private[dependency] type ProjectNode = (
  projectName: ProjectName,
  sourcePaths: Set[Path],
  sourceDependencies: Set[ProjectName],
  artifactDependencies: Set[MavenArtifactDependency]
)

private[joern] case class DependencyGraph(
  nodes: Map[ProjectName, ProjectNode],
  dependents: Map[ProjectName, Set[ProjectName]],
  dependencies: Map[ProjectName, Set[ProjectName]],
  cacheDir: Path
) {

  private[joern] def transitiveDependenciesForProjectsInDir(
    inputDir: Path
  ): DependencyResolverV2.DependencyResolutionResult = {
    val checked     = mutable.Set[String]()
    val toCheck     = mutable.Stack[String]()
    val sourcePaths = mutable.Set[Path]()
    val artifacts   = mutable.Map[DependencyGraph.ArtifactIdentity, MavenArtifactDependency]()

    // Resolve through symlinks before comparing — the input path may have been built relative
    // to e.g. a symlinked source root, while Gradle always reports the real path.
    def realPath(path: Path): Path =
      Try(path.toRealPath()).getOrElse(path.toAbsolutePath.normalize())

    val resolvedInputDir = realPath(inputDir)
    val matchedProjects = nodes.valuesIterator.filter { project =>
      project.sourcePaths.exists(sourcePath => realPath(sourcePath).startsWith(resolvedInputDir))
    }.toList

    val seedProjects =
      if (matchedProjects.nonEmpty) matchedProjects
      else {
        logger.info(
          s"No project's source paths fell under $resolvedInputDir; seeding from every project in the graph " +
            s"(${nodes.size} nodes). This covers shared source directories wired by relative path and KMP " +
            s"`dependsOn` cross-build links, where recorded source paths may live outside the input dir."
        )
        nodes.valuesIterator.toList
      }
    seedProjects.foreach(project => toCheck.push(project.projectName))

    while (toCheck.nonEmpty) {
      val projectName = toCheck.pop()

      if (!checked.contains(projectName)) {
        checked.add(projectName)

        nodes.get(projectName) match {
          case Some(project) =>
            sourcePaths.addAll(project.sourcePaths)
            project.artifactDependencies.foreach(DependencyGraph.mergeArtifact(artifacts, _))
            project.sourceDependencies.foreach(toCheck.push)

          case None =>
            logger.debug(s"Project $projectName not found in dependency graph")
        }
      }
    }

    val artifactPaths = artifacts.valuesIterator.map(_.path).toSet
    (sourcePaths.toSet, AarExtractor.materializeJars(artifactPaths, cacheDir))
  }

}

private[joern] object DependencyGraph {

  private val logger = LoggerFactory.getLogger(this.getClass)

  private[dependency] type ArtifactIdentity = (Option[String], String, Option[String], String)

  /** Insert `next` into `acc`, keeping the higher Maven version when an artifact with the same identity is already
    * present. A missing version loses to any present version; if both are missing, the existing entry wins.
    */
  private def mergeArtifact(
    acc: mutable.Map[ArtifactIdentity, MavenArtifactDependency],
    next: MavenArtifactDependency
  ): Unit = {
    acc.get(next.identity) match {
      case None => acc.put(next.identity, next)
      case Some(existing) if isNewer(next.version, existing.version) =>
        logger.debug(
          s"Replacing ${existing.group.getOrElse("?")}:${existing.name}:${existing.version.getOrElse("?")} " +
            s"with version ${next.version.getOrElse("?")}"
        )
        acc.put(next.identity, next)
      case _ => ()
    }
  }

  private def isNewer(candidate: Option[String], current: Option[String]): Boolean = {
    (candidate, current) match {
      case (Some(c), Some(e)) => new ComparableVersion(c).compareTo(new ComparableVersion(e)) > 0
      case (Some(_), None)    => true
      case _                  => false
    }
  }

  // The init script writes the optional fields as JSON null when unknown (e.g. the
  // classifier). Treat null as absent.
  private def optStr(obj: ujson.Obj, key: String): Option[String] = {
    obj.value.get(key).flatMap(value => if (value.isNull) None else Some(value.str))
  }

  private def parseJson(path: Path): Option[ProjectNode] = {
    logger.info(s"Attempting to parse dependency fetcher JSON from $path")
    Try {
      val projectMap = ujson.read(path).obj

      val projectName = projectMap("projectName").str

      val sourcePaths = projectMap("sourceRoots").arr.map(pathStr => Path.of(pathStr.str)).toSet

      val sourceDependencies = projectMap("sourceDependencies").arr.map(_.str).toSet

      val artifactDependencies = projectMap("artifactDependencies").arr.map { depJson =>
        val depObj = depJson.obj

        val path       = Path.of(depObj("path").str)
        val name       = optStr(depObj, "name").getOrElse(path.getFileName.toString)
        val group      = optStr(depObj, "group")
        val version    = optStr(depObj, "version")
        val classifier = optStr(depObj, "classifier")
        val extension  = optStr(depObj, "extension").getOrElse("")

        MavenArtifactDependency(path, group, name, version, classifier, extension)
      }.toSet

      (projectName, sourcePaths, sourceDependencies, artifactDependencies)

    } match {
      case Success(node) =>
        logger.info(s"Successfully parsed dependency fetcher JSON")
        Option(node)

      case Failure(ex) =>
        logger.warn(s"Encountered exception attempting to parse dependency json at $path", ex)
        None
    }
  }

  def fromJson(cacheDir: Path, jsonPaths: Array[Path]): DependencyGraph = {
    val nodes        = mutable.Map[String, ProjectNode]()
    val dependents   = mutable.Map[String, Set[String]]()
    val dependencies = mutable.Map[String, Set[String]]()

    jsonPaths.flatMap(parseJson).foreach { project =>
      val projectName = project.projectName
      if (nodes.contains(projectName)) {
        logger.warn(s"Encountered duplicate project definition: $projectName. Overwriting")
      }
      nodes.put(projectName, project)

      project.sourceDependencies.foreach { dependencyName =>
        dependencies.put(projectName, dependencies.getOrElse(projectName, Set[String]()) + dependencyName)

        dependents.put(dependencyName, dependents.getOrElse(dependencyName, Set[String]()) + projectName)
      }
    }

    DependencyGraph(nodes.toMap, dependents.toMap, dependencies.toMap, cacheDir)
  }
}
