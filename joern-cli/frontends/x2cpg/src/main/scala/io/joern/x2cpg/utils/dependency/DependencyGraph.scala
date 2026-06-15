package io.joern.x2cpg.utils.dependency

import io.joern.x2cpg.utils.dependency.DependencyGraph.logger
import org.slf4j.LoggerFactory
import ujson.Value

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

  override def hashCode(): Int = (group, name, version, classifier, extension).hashCode()

  override def equals(other: Any): Boolean = {
    other match {
      case otherDep: MavenArtifactDependency =>
        (
          name,
          group,
          version,
          classifier,
          extension
        ) == (otherDep.name, otherDep.group, otherDep.version, otherDep.classifier, otherDep.extension)

      case _ => false
    }
  }
}

private[dependency] type ProjectNode = (
  projectName: ProjectName,
  sourcePaths: Set[Path],
  sourceDependencies: Set[ProjectName],
  artifactDependencies: Set[MavenArtifactDependency]
)

private[joern] type DependencyResult = (artifacts: Set[Path], sources: Set[Path])

private[joern] case class DependencyGraph(
  nodes: Map[ProjectName, ProjectNode],
  dependents: Map[ProjectName, Set[ProjectName]],
  dependencies: Map[ProjectName, Set[ProjectName]],
  cacheDir: Path
) {

  private[joern] def transitiveDependenciesForProjectsInDir(inputDir: Path): DependencyResult = {
    val checked       = mutable.Set[String]()
    val toCheck       = mutable.Stack[String]()
    val sourcePaths   = mutable.Set[Path]()
    val artifactPaths = mutable.Set[Path]()

    // Resolve through symlinks before comparing — the input path may have been built relative
    // to e.g. a symlinked source root, while Gradle always reports the real path.
    def realPath(p: Path): Path =
      Try(p.toRealPath()).getOrElse(p.toAbsolutePath.normalize())

    val resolvedInputDir = realPath(inputDir)
    nodes.valuesIterator
      .filter { project =>
        project.sourcePaths.exists(sourcePath => realPath(sourcePath).startsWith(resolvedInputDir))
      }
      .foreach(project => toCheck.push(project.projectName))

    while (toCheck.nonEmpty) {
      val projectName = toCheck.pop

      if (!checked.contains(projectName)) {
        checked.add(projectName)

        nodes.get(projectName) match {
          case Some(project) =>
            sourcePaths.addAll(project.sourcePaths)
            artifactPaths.addAll(project.artifactDependencies.map(_.path))
            project.sourceDependencies.foreach(toCheck.push)

          case None =>
            logger.debug(s"Project $projectName not found in dependency graph")
        }
      }
    }

    (AarExtractor.materializeJars(artifactPaths, cacheDir), sourcePaths.toSet)
  }

}

private[joern] object DependencyGraph {

  private val logger = LoggerFactory.getLogger(this.getClass)

  // The init script writes the optional fields as JSON null when unknown (e.g. the
  // classifier). Treat null as absent.
  private def optStr(obj: ujson.Obj, key: String): Option[String] = {
    obj.value.get(key).flatMap(v => if (v.isNull) None else Some(v.str))
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
    val jsonObjMap   = mutable.Map[String, Value]()

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
