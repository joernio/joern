package io.joern.x2cpg.utils.dependency

import io.joern.x2cpg.utils.dependency.{DependencyResolverV2, DependencyResolver}
import io.shiftleft.utils.ProjectRoot
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Path, Paths}

/** In-depth tests of the V2 dependency fetcher and source-dir override logic against the `gradle-test-project` fixture.
  * Each test fans the gradle init script out, parses the resulting depInfo JSONs, and asserts the source roots returned
  * for a given input path.
  */
class DependencyResolverV2Tests extends AnyWordSpec with Matchers {

  private def graphFor(inputPath: Path) =
    DependencyResolverV2
      .getDependencyGraph(inputPath)
      .getOrElse(fail(s"V2 dependency resolver returned no graph for $inputPath"))

  "V2 dependency fetcher against the gradle-test-project" should {
    val projectDir: Path =
      Paths
        .get(ProjectRoot.relativise("joern-cli/frontends/x2cpg/src/test/resources/code/gradle-test-project"))
        .toAbsolutePath
        .normalize()

    def sortedModulesIn(sourceDirs: Set[Path]): List[String] = {
      // The names of all the subprojects we expect to find in the test project
      val allModules =
        Set("sharedUtils", "core", "lib", "testLib", "clientCore", "client", "server", "emptyModule", "mixedSources")
      sourceDirs.toList.flatMap { p =>
        // sourceDirs are absolute paths like .../gradle-test-project/core/src/main/kotlin
        val s = p.toAbsolutePath.normalize().toString
        allModules.find(m => s.contains(s"/$m/src/"))
      }.sorted
    }

    "include every settings.gradle.kts subproject when invoked from the root" in {
      val graph        = graphFor(projectDir)
      val (_, sources) = graph.transitiveDependenciesForProjectsInDir(projectDir)
      val modules      = sortedModulesIn(sources)
      // emptyModule has no src/ dir, so it won't surface in sources.
      // mixedSources is listed twice since it is captured once for java sources and once for kotlin
      modules shouldBe List(
        "client",
        "clientCore",
        "core",
        "lib",
        "mixedSources",
        "mixedSources",
        "server",
        "sharedUtils",
        "testLib"
      )
      // ghostModule must NOT appear — it's absent from settings.gradle.kts.
      modules should not contain "ghostModule"
    }

    "from the `client` subproject include only client → clientCore → core → sharedUtils" in {
      val clientDir    = projectDir.resolve("client")
      val graph        = graphFor(clientDir)
      val (_, sources) = graph.transitiveDependenciesForProjectsInDir(clientDir)
      sortedModulesIn(sources) shouldBe List("client", "clientCore", "core", "sharedUtils")
    }

    "from the `server` diamond produce a deduplicated closure" in {
      val serverDir    = projectDir.resolve("server")
      val graph        = graphFor(serverDir)
      val (_, sources) = graph.transitiveDependenciesForProjectsInDir(serverDir)
      val modules      = sortedModulesIn(sources)
      // server → core → sharedUtils
      // server → lib  → sharedUtils  (the diamond bottom)
      // server → testLib(test) → lib (only at test scope, so testLib should not be picked up)
      sortedModulesIn(sources) shouldBe List("core", "lib", "server", "sharedUtils")
    }

    "from `mixedSources` expose both kotlin and java source roots" in {
      val mixedDir     = projectDir.resolve("mixedSources")
      val graph        = graphFor(mixedDir)
      val (_, sources) = graph.transitiveDependenciesForProjectsInDir(mixedDir)
      val sourcesStr   = sources.map(_.toString)
      sourcesStr.exists(_.endsWith("/mixedSources/src/main/kotlin")) shouldBe true
      sourcesStr.exists(_.endsWith("/mixedSources/src/main/java")) shouldBe true
    }

    "produce a graph node for `emptyModule` but no source roots" in {
      val graph = graphFor(projectDir)
      graph.nodes.keys should contain(":emptyModule")
      val emptyNode = graph.nodes(":emptyModule")
      emptyNode.sourcePaths.shouldBe(empty)
    }

    "fall back (return None) when invoked from `ghostModule`'s directory" in {
      val ghostDir = projectDir.resolve("ghostModule")
      val result   = DependencyResolverV2.resolve(ghostDir.toString, flagFromConfig = true)
      // ghostModule is not part of the Gradle build, so the fetcher resolves a graph that
      // does not contain ghostModule, no source dir is contained within `ghostDir`, and the
      // helper returns None to trigger the frontend fallback.
      result shouldBe None
    }

    "fall back when invoked from `emptyModule` (no sources for the input path)" in {
      val emptyDir = projectDir.resolve("emptyModule")
      val result   = DependencyResolverV2.resolve(emptyDir.toString, flagFromConfig = true)
      result shouldBe None
    }
  }

  // These tests need the correct combination of JDK, Gradle version and Android SDK available.
  // Un-ignore for local testing
  "V2 dependency resolver against gradle-android-test" ignore {
    val projectDir: Path =
      Paths
        .get(ProjectRoot.relativise("joern-cli/frontends/x2cpg/src/test/resources/code/gradle-android-test"))
        .toAbsolutePath
        .normalize()

    "find the app subproject and used lib when resolving from the app subproject" in {
      val graph   = graphFor(projectDir.resolve("app"))
      val appNode = graph.nodes(":app")

      appNode.sourceDependencies shouldBe Set(":used-lib")
      appNode.sourcePaths shouldBe Set(
        projectDir.resolve("app/src/main/java"),
        projectDir.resolve("app/src/main/kotlin")
      )

      val usedLibNode = graph.nodes(":used-lib")
      usedLibNode.sourceDependencies shouldBe Set()
      usedLibNode.sourcePaths shouldBe Set(
        projectDir.resolve("used-lib/src/main/java"),
        projectDir.resolve("used-lib/src/main/kotlin")
      )

      graph.nodes.keys should contain theSameElementsAs List(":app", ":used-lib")
    }

    "find all subprojects when resolving from the project root" in {
      val graph = graphFor(projectDir)
      graph.nodes.keys should contain theSameElementsAs List(":app", ":used-lib", ":unused-lib", ":")
    }

    // AGP only populates `android.bootClasspath` / `androidApis` when it can
    // locate an Android SDK — requires ANDROID_HOME (or sdk.dir in
    // local.properties) to point at a real SDK with platforms/android-N
    // installed. The test relies on whichever environment the suite is run in
    // supplying that; with no SDK available the bootClasspath comes back
    // empty and this case fails fast
    "include android.jar from the Android boot classpath in the app's artifact deps" in {
      val graph       = graphFor(projectDir.resolve("app"))
      val appNode     = graph.nodes(":app")
      val androidJars = appNode.artifactDependencies.filter(_.path.getFileName.toString == "android.jar")
      androidJars should have size 1
      val androidJar = androidJars.head
      androidJar.group shouldBe None
      androidJar.version shouldBe None
      androidJar.classifier shouldBe None
      androidJar.extension shouldBe "jar"
    }
  }

  "V2 dependency resolver against gradle-nested-module-test" should {
    val projectDir: Path =
      Paths
        .get(ProjectRoot.relativise("joern-cli/frontends/x2cpg/src/test/resources/code/gradle-nested-module-test"))
        .toAbsolutePath
        .normalize()

    "find :core and :core:lib when resolving from core" in {
      val graph = graphFor(projectDir.resolve("core"))

      val coreNode = graph.nodes(":core")
      coreNode.sourceDependencies shouldBe Set()
      coreNode.sourcePaths shouldBe Set(projectDir.resolve("core/src/main/kotlin"))

      val libNode = graph.nodes(":core:lib")
      libNode.sourceDependencies shouldBe Set(":core")
      libNode.sourcePaths shouldBe Set(projectDir.resolve("core/lib/src/main/kotlin"))

      graph.nodes.keys should contain theSameElementsAs List(":core", ":core:lib")
    }
  }

  "V2 dependency resolver against gradle-kmp-test" should {
    val projectDir: Path =
      Paths
        .get(ProjectRoot.relativise("joern-cli/frontends/x2cpg/src/test/resources/code/gradle-kmp-test"))
        .toAbsolutePath
        .normalize()

    "find only used-lib when resolving from the used-lib subproject" in {
      val graph       = graphFor(projectDir.resolve("used-lib"))
      val usedLibNode = graph.nodes(":used-lib")

      usedLibNode.sourceDependencies shouldBe Set()
      usedLibNode.sourcePaths shouldBe Set(
        projectDir.resolve("used-lib/src/commonMain/kotlin"),
        projectDir.resolve("used-lib/src/jvmMain/kotlin"),
        projectDir.resolve("used-lib/src/nativeMain/kotlin")
      )

      graph.nodes.keys should contain theSameElementsAs List(":used-lib")
    }

    "find all subprojects when resolving from the project root" in {
      val graph    = graphFor(projectDir)
      val rootNode = graph.nodes(":")

      rootNode.sourceDependencies shouldBe Set(":used-lib")
      rootNode.sourcePaths shouldBe Set(
        projectDir.resolve("src/commonMain/kotlin"),
        projectDir.resolve("src/jvmMain/kotlin"),
        projectDir.resolve("src/nativeMain/kotlin")
      )

      val usedLibNode = graph.nodes(":used-lib")
      usedLibNode.sourceDependencies shouldBe Set()
      usedLibNode.sourcePaths shouldBe Set(
        projectDir.resolve("used-lib/src/commonMain/kotlin"),
        projectDir.resolve("used-lib/src/jvmMain/kotlin"),
        projectDir.resolve("used-lib/src/nativeMain/kotlin")
      )

      graph.nodes.keys should contain theSameElementsAs List(":", ":used-lib", ":unused-lib")
    }
  }
}
