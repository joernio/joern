package io.joern.x2cpg.utils.dependency

import io.joern.x2cpg.utils.dependency.DependencyResolverV2
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

  private def projectRoot(name: String): Path =
    Paths.get(ProjectRoot.relativise(s"joern-cli/frontends/x2cpg/src/test/resources/code/$name"))
      .toAbsolutePath
      .normalize()

  private def sourcesFor(inputDir: Path): Set[Path] =
    graphFor(inputDir).transitiveDependenciesForProjectsInDir(inputDir).sourceDirs

  "V2 dependency fetcher against the gradle-test-project" should {
    val projectDir: Path = projectRoot("gradle-test-project")

    "include every settings.gradle.kts subproject when invoked from the root" in {
      val sources = sourcesFor(projectDir)
      // emptyModule has no src/ dir, so it won't surface in sources.
      // mixedSources contributes two roots — one for kotlin, one for java.
      // ghostModule is absent from settings.gradle.kts, so it must not appear.
      sources should contain theSameElementsAs Set(
        projectDir.resolve("client/src/main/kotlin"),
        projectDir.resolve("clientCore/src/main/kotlin"),
        projectDir.resolve("core/src/main/kotlin"),
        projectDir.resolve("lib/src/main/kotlin"),
        projectDir.resolve("mixedSources/src/main/kotlin"),
        projectDir.resolve("mixedSources/src/main/java"),
        projectDir.resolve("server/src/main/kotlin"),
        projectDir.resolve("sharedUtils/src/main/kotlin"),
        projectDir.resolve("testLib/src/main/kotlin")
      )
    }

    "from the `client` subproject include only client → clientCore → core → sharedUtils" in {
      val clientDir = projectDir.resolve("client")
      val sources   = sourcesFor(clientDir)
      sources should contain theSameElementsAs Set(
        projectDir.resolve("client/src/main/kotlin"),
        projectDir.resolve("clientCore/src/main/kotlin"),
        projectDir.resolve("core/src/main/kotlin"),
        projectDir.resolve("sharedUtils/src/main/kotlin")
      )
    }

    "from the `server` diamond produce a deduplicated closure" in {
      val serverDir = projectDir.resolve("server")
      val sources   = sourcesFor(serverDir)
      // server → core → sharedUtils
      // server → lib  → sharedUtils  (the diamond bottom)
      // server → testLib(test) → lib (only at test scope, so testLib should not be picked up)
      sources should contain theSameElementsAs Set(
        projectDir.resolve("server/src/main/kotlin"),
        projectDir.resolve("core/src/main/kotlin"),
        projectDir.resolve("lib/src/main/kotlin"),
        projectDir.resolve("sharedUtils/src/main/kotlin")
      )
    }

    "from `mixedSources` expose both kotlin and java source roots" in {
      val mixedDir = projectDir.resolve("mixedSources")
      val sources  = sourcesFor(mixedDir)
      sources should contain(projectDir.resolve("mixedSources/src/main/kotlin"))
      sources should contain(projectDir.resolve("mixedSources/src/main/java"))
    }

    "produce a graph node for `emptyModule` but no source roots" in {
      val graph = graphFor(projectDir)
      graph.nodes.keys should contain(":emptyModule")
      val emptyNode = graph.nodes(":emptyModule")
      emptyNode.sourcePaths.shouldBe(empty)
    }

    "fall back (return None) when invoked from `ghostModule`'s directory" in {
      val ghostDir = projectDir.resolve("ghostModule")
      val result   = DependencyResolverV2.resolve(ghostDir.toString)
      // ghostModule is not part of the Gradle build, so the fetcher resolves a graph that
      // does not contain ghostModule, no source dir is contained within `ghostDir`, and the
      // helper returns None to trigger the frontend fallback.
      result shouldBe None
    }

    "fall back when invoked from `emptyModule` (no sources for the input path)" in {
      val emptyDir = projectDir.resolve("emptyModule")
      val result   = DependencyResolverV2.resolve(emptyDir.toString)
      result shouldBe None
    }

    // Full-graph snapshot. Asserts both source-dep edges and that every kotlin-jvm subproject
    // pulls kotlin-stdlib as a direct external. Versions are intentionally not asserted —
    // they're a function of the kotlin-jvm plugin version pinned in the root build file.
    "expose the full graph (source deps + artifact coords) for every node" in {
      val graph = graphFor(projectDir)

      graph.nodes.keys should contain theSameElementsAs Set(
        ":",
        ":client",
        ":clientCore",
        ":core",
        ":emptyModule",
        ":lib",
        ":mixedSources",
        ":server",
        ":sharedUtils",
        ":testLib"
      )

      // Root applies no plugin and has no resolvable classpath configuration, so neither
      // source-dep walk nor external-dep walk surfaces anything.
      graph.nodes(":").sourceDependencies shouldBe Set.empty[String]

      // testImplementation deps live on testCompileClasspath, which is excluded by
      // `validConfigurations` — so :testLib never appears as a source-dep of :clientCore
      // or :server even though they declare it at test scope.
      graph.nodes(":client").sourceDependencies shouldBe Set(":clientCore")
      graph.nodes(":clientCore").sourceDependencies shouldBe Set(":core")
      graph.nodes(":core").sourceDependencies shouldBe Set(":sharedUtils")
      graph.nodes(":emptyModule").sourceDependencies shouldBe Set.empty[String]
      graph.nodes(":lib").sourceDependencies shouldBe Set(":sharedUtils")
      graph.nodes(":mixedSources").sourceDependencies shouldBe Set.empty[String]
      graph.nodes(":server").sourceDependencies shouldBe Set(":core", ":lib")
      graph.nodes(":sharedUtils").sourceDependencies shouldBe Set.empty[String]
      graph.nodes(":testLib").sourceDependencies shouldBe Set(":lib")

      // Root has no resolvable classpath config — no externals.
      graph.nodes(":").artifactDependencies shouldBe Set.empty[MavenArtifactDependency]

      // Every kotlin-jvm subproject pulls kotlin-stdlib directly (added by the plugin).
      // Transitives like org.jetbrains:annotations don't appear because
      // `getExternalDependencyNames` walks only the rootVariant's immediate children.
      val kotlinStdlib = (Some("org.jetbrains.kotlin"), "kotlin-stdlib")
      val kotlinJvmSubprojects = List(
        ":client",
        ":clientCore",
        ":core",
        ":emptyModule",
        ":lib",
        ":mixedSources",
        ":server",
        ":sharedUtils",
        ":testLib"
      )
      kotlinJvmSubprojects.foreach { name =>
        val coords = graph.nodes(name).artifactDependencies.map(d => (d.group, d.name))
        withClue(s"$name should pull kotlin-stdlib directly: ") {
          coords should contain(kotlinStdlib)
        }
      }
    }
  }

  // These tests need the correct combination of JDK, Gradle version and Android SDK available.
  // Un-ignore for local testing
  "V2 dependency resolver against gradle-android-test" ignore {
    val projectDir: Path = projectRoot("gradle-android-test")

    "from the `app` subproject pull in app and used-lib (but not unused-lib)" in {
      val appDir  = projectDir.resolve("app")
      val sources = sourcesFor(appDir)
      sources should contain theSameElementsAs Set(
        projectDir.resolve("app/src/main/java"),
        projectDir.resolve("app/src/main/kotlin"),
        projectDir.resolve("used-lib/src/main/java"),
        projectDir.resolve("used-lib/src/main/kotlin")
      )
    }

    "from the project root include every subproject's source roots" in {
      val sources = sourcesFor(projectDir)
      sources should contain theSameElementsAs Set(
        projectDir.resolve("app/src/main/java"),
        projectDir.resolve("app/src/main/kotlin"),
        projectDir.resolve("used-lib/src/main/java"),
        projectDir.resolve("used-lib/src/main/kotlin"),
        projectDir.resolve("unused-lib/src/main/java"),
        projectDir.resolve("unused-lib/src/main/kotlin")
      )
    }

    // AGP only populates `android.bootClasspath` / `androidApis` when it can
    // locate an Android SDK — requires ANDROID_HOME (or sdk.dir in
    // local.properties) to point at a real SDK with platforms/android-N
    // installed. The test relies on whichever environment the suite is run in
    // supplying that; with no SDK available the bootClasspath comes back
    // empty and this case fails fast.
    //
    // Reads through `graph.nodes(":app")` rather than the transitive helper because
    // we need the MavenArtifactDependency object (with its null coordinate fields)
    // — the transitive walk only exposes paths.
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

    // Full-graph snapshot. Asserts source-dep edges and that each AGP-applying subproject
    // pulls android.jar (via the Android branch) and kotlin-stdlib (via the kotlin plugin).
    "expose the full graph (source deps + android.jar + kotlin-stdlib) for every node" in {
      val graph = graphFor(projectDir)

      graph.nodes.keys should contain theSameElementsAs Set(":", ":app", ":used-lib", ":unused-lib")

      // Root applies no AGP / kotlin plugin — no source-dep walk, no externals.
      graph.nodes(":").sourceDependencies shouldBe Set.empty[String]
      graph.nodes(":").artifactDependencies shouldBe Set.empty[MavenArtifactDependency]

      graph.nodes(":app").sourceDependencies shouldBe Set(":used-lib")
      graph.nodes(":used-lib").sourceDependencies shouldBe Set.empty[String]
      graph.nodes(":unused-lib").sourceDependencies shouldBe Set.empty[String]

      val androidJar     = (None: Option[String], "android.jar")
      val kotlinStdlib   = (Some("org.jetbrains.kotlin"), "kotlin-stdlib")
      val androidModules = List(":app", ":used-lib", ":unused-lib")
      androidModules.foreach { name =>
        val coords = graph.nodes(name).artifactDependencies.map(d => (d.group, d.name))
        withClue(s"$name should pull both android.jar and kotlin-stdlib: ") {
          coords should contain(androidJar)
          coords should contain(kotlinStdlib)
        }
      }
    }
  }

  "V2 dependency resolver against gradle-nested-module-test" should {
    val projectDir: Path = projectRoot("gradle-nested-module-test")

    "from `core` pull in :core and the nested :core:lib subproject" in {
      val coreDir = projectDir.resolve("core")
      val sources = sourcesFor(coreDir)
      sources should contain theSameElementsAs Set(
        projectDir.resolve("core/src/main/kotlin"),
        projectDir.resolve("core/lib/src/main/kotlin")
      )
    }

    "expose the full graph (source deps + kotlin-stdlib) for every node" in {
      val graph = graphFor(projectDir)

      graph.nodes.keys should contain theSameElementsAs Set(":", ":core", ":core:lib", ":main")

      graph.nodes(":").sourceDependencies shouldBe Set.empty[String]
      graph.nodes(":core").sourceDependencies shouldBe Set.empty[String]
      graph.nodes(":core:lib").sourceDependencies shouldBe Set(":core")
      graph.nodes(":main").sourceDependencies shouldBe Set(":core:lib")

      // Root applies no plugin — no externals.
      graph.nodes(":").artifactDependencies shouldBe Set.empty[MavenArtifactDependency]

      val kotlinStdlib = (Some("org.jetbrains.kotlin"), "kotlin-stdlib")
      List(":core", ":core:lib", ":main").foreach { name =>
        val coords = graph.nodes(name).artifactDependencies.map(d => (d.group, d.name))
        withClue(s"$name should pull kotlin-stdlib directly: ") {
          coords should contain(kotlinStdlib)
        }
      }
    }
  }

  "V2 dependency resolver against gradle-kmp-test" should {
    val projectDir: Path = projectRoot("gradle-kmp-test")

    "from the `used-lib` subproject expose its KMP source roots only" in {
      val usedLibDir = projectDir.resolve("used-lib")
      val sources    = sourcesFor(usedLibDir)
      sources should contain theSameElementsAs Set(
        projectDir.resolve("used-lib/src/commonMain/kotlin"),
        projectDir.resolve("used-lib/src/jvmMain/kotlin"),
        projectDir.resolve("used-lib/src/nativeMain/kotlin")
      )
    }

    "from the project root include every subproject's KMP source roots" in {
      val sources = sourcesFor(projectDir)
      sources should contain theSameElementsAs Set(
        projectDir.resolve("src/commonMain/kotlin"),
        projectDir.resolve("src/jvmMain/kotlin"),
        projectDir.resolve("src/nativeMain/kotlin"),
        projectDir.resolve("used-lib/src/commonMain/kotlin"),
        projectDir.resolve("used-lib/src/jvmMain/kotlin"),
        projectDir.resolve("used-lib/src/nativeMain/kotlin"),
        projectDir.resolve("unused-lib/src/commonMain/kotlin"),
        projectDir.resolve("unused-lib/src/jvmMain/kotlin"),
        projectDir.resolve("unused-lib/src/nativeMain/kotlin")
      )
    }

    // KMP-only source-dep wiring: project-typed deps are declared on per-source-set
    // configurations (e.g. `commonMainImplementation`), which do NOT propagate to the
    // resolvable classpath configurations `validConfigurations` filters to. They flow
    // in through `collectKmpSourceRootsAndDeps`.
    //
    // Artifact deps are NOT asserted here: which resolvable configurations a KMP project
    // exposes (`compileClasspath`/`jvmCompileClasspath`/per-target variants) is sensitive
    // to the kotlin-multiplatform plugin version and would make this test brittle.
    "expose the full graph (KMP source-dep edges) for every node" in {
      val graph = graphFor(projectDir)

      graph.nodes.keys should contain theSameElementsAs Set(":", ":used-lib", ":unused-lib")

      graph.nodes(":").sourceDependencies shouldBe Set(":used-lib")
      graph.nodes(":used-lib").sourceDependencies shouldBe Set.empty[String]
      graph.nodes(":unused-lib").sourceDependencies shouldBe Set.empty[String]
    }
  }
}
