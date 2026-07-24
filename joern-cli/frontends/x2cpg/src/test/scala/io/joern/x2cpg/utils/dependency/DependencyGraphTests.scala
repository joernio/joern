package io.joern.x2cpg.utils.dependency

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Path, Paths}

class DependencyGraphTests extends AnyWordSpec with Matchers {

  import DependencyGraphTests.*

  "transitiveDependenciesForProjectsInDir" should {

    "deduplicate two sibling projects depending on the same artifact, keeping the newer version" in {
      val older = artifact("com.example", "lib", "1.0.0")
      val newer = artifact("com.example", "lib", "2.0.0")

      val graph = buildGraph(
        project("a", sourcePath = "/repo/a", artifacts = Set(older)),
        project("b", sourcePath = "/repo/b", artifacts = Set(newer))
      )

      val (_, artifacts) = graph.transitiveDependenciesForProjectsInDir(Paths.get("/repo"))
      artifacts shouldBe Set(newer.path)
    }

    "pick the newer version regardless of traversal order" in {
      val older = artifact("com.example", "lib", "1.0.0")
      val newer = artifact("com.example", "lib", "2.0.0")

      // Reverse argument order to flush out any first-wins bug.
      val graph = buildGraph(
        project("a", sourcePath = "/repo/a", artifacts = Set(newer)),
        project("b", sourcePath = "/repo/b", artifacts = Set(older))
      )

      val (_, artifacts) = graph.transitiveDependenciesForProjectsInDir(Paths.get("/repo"))
      artifacts shouldBe Set(newer.path)
    }

    "treat Maven version ordering correctly (1.10 > 1.9, not string-compared)" in {
      val v19  = artifact("com.example", "lib", "1.9")
      val v110 = artifact("com.example", "lib", "1.10")

      val graph = buildGraph(
        project("a", sourcePath = "/repo/a", artifacts = Set(v19)),
        project("b", sourcePath = "/repo/b", artifacts = Set(v110))
      )

      val (_, artifacts) = graph.transitiveDependenciesForProjectsInDir(Paths.get("/repo"))
      artifacts shouldBe Set(v110.path)
    }

    "rank release above pre-release (1.0 > 1.0-SNAPSHOT)" in {
      val snapshot = artifact("com.example", "lib", "1.0-SNAPSHOT")
      val release  = artifact("com.example", "lib", "1.0")

      val graph = buildGraph(
        project("a", sourcePath = "/repo/a", artifacts = Set(snapshot)),
        project("b", sourcePath = "/repo/b", artifacts = Set(release))
      )

      val (_, artifacts) = graph.transitiveDependenciesForProjectsInDir(Paths.get("/repo"))
      artifacts shouldBe Set(release.path)
    }

    "keep both artifacts when classifier differs (different identity)" in {
      val main    = artifact("com.example", "lib", "1.0.0", classifier = None)
      val sources = artifact("com.example", "lib", "1.0.0", classifier = Some("sources"))

      val graph = buildGraph(project("a", sourcePath = "/repo/a", artifacts = Set(main, sources)))

      val (_, artifacts) = graph.transitiveDependenciesForProjectsInDir(Paths.get("/repo"))
      artifacts shouldBe Set(main.path, sources.path)
    }

    // This should not come up in real projects, but this test demonstrates existing behaviour
    // in case it is ever relevant
    "keep both artifacts when extension differs (different identity)" in {
      val jar = artifact("com.example", "lib", "1.0.0", extension = "jar")
      val pom = artifact("com.example", "lib", "1.0.0", extension = "pom")

      val graph = buildGraph(project("a", sourcePath = "/repo/a", artifacts = Set(jar, pom)))

      val (_, artifacts) = graph.transitiveDependenciesForProjectsInDir(Paths.get("/repo"))
      artifacts shouldBe Set(jar.path, pom.path)
    }

    "keep both artifacts when group differs but name matches" in {
      val foo = artifact("com.example", "lib", "1.0.0")
      val bar = artifact("org.other", "lib", "1.0.0")

      val graph = buildGraph(project("a", sourcePath = "/repo/a", artifacts = Set(foo, bar)))

      val (_, artifacts) = graph.transitiveDependenciesForProjectsInDir(Paths.get("/repo"))
      artifacts shouldBe Set(foo.path, bar.path)
    }

    "prefer a present version over a missing one" in {
      val versioned   = artifact("com.example", "lib", "1.0.0")
      val unversioned = versioned.copy(path = Paths.get("/cache/unknown/lib.jar"), version = None)

      val graph = buildGraph(
        project("a", sourcePath = "/repo/a", artifacts = Set(unversioned)),
        project("b", sourcePath = "/repo/b", artifacts = Set(versioned))
      )

      val (_, artifacts) = graph.transitiveDependenciesForProjectsInDir(Paths.get("/repo"))
      artifacts shouldBe Set(versioned.path)
    }

  }
}

private object DependencyGraphTests {

  /** Build a synthetic MavenArtifactDependency with a path derived from coordinates so distinct versions get distinct
    * paths (matching real cache layout).
    */
  def artifact(
    group: String,
    name: String,
    version: String,
    classifier: Option[String] = None,
    extension: String = "jar"
  ): MavenArtifactDependency = {
    val classifierSuffix = classifier.map("-" + _).getOrElse("")
    val path             = Paths.get(s"/cache/$group/$name/$version/$name-$version$classifierSuffix.$extension")
    MavenArtifactDependency(path, Some(group), name, Some(version), classifier, extension)
  }

  def project(
    name: String,
    sourcePath: String,
    artifacts: Set[MavenArtifactDependency],
    sourceDependencies: Set[String] = Set.empty
  ): ProjectNode =
    (name, Set(Paths.get(sourcePath)), sourceDependencies, artifacts)

  def buildGraph(projects: ProjectNode*): DependencyGraph = {
    val nodes        = projects.map(p => p.projectName -> p).toMap
    val dependencies = projects.map(p => p.projectName -> p.sourceDependencies).toMap
    val dependents = projects
      .flatMap(p => p.sourceDependencies.map(dep => dep -> p.projectName))
      .groupMap(_._1)(_._2)
      .view
      .mapValues(_.toSet)
      .toMap
    DependencyGraph(nodes, dependents, dependencies, cacheDir = Paths.get("/cache"))
  }
}
