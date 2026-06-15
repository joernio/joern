package io.joern.kotlin2cpg.dependency

import io.joern.kotlin2cpg.{Config, Kotlin2Cpg, Main}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.utils.ProjectRoot
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Path, Paths}
import scala.util.{Failure, Success}

class DependencyResolverV2Tests extends AnyWordSpec with Matchers {

  private val projectDir: Path =
    Paths
      .get(ProjectRoot.relativise("joern-cli/frontends/x2cpg/src/test/resources/code/gradle_test_project"))
      .toAbsolutePath
      .normalize()

  "kotlin2cpg when using the dependency fetcher V2" should {

    "handle a subproject correctly" in {
      val subprojectPath = projectDir.resolve("client").toString
      val config =
        Config().withDownloadDependencies(true).withEnableDependencyResolverV2(true).withInputPath(subprojectPath)

      Kotlin2Cpg().createCpg(config) match {
        case Success(cpg) =>
          cpg.file.name.map(Path.of(_)).toSet shouldBe Set(
            "src/main/kotlin/com/example/client/Client.kt",
            "../clientCore/src/main/kotlin/com/example/clientcore/ClientCore.kt",
            "../core/src/main/kotlin/com/example/core/CoreConstants.kt",
            "../core/src/main/kotlin/com/example/core/CoreService.kt",
            "../sharedUtils/src/main/kotlin/com/example/sharedutils/SharedUtils.kt"
          ).map(Path.of(_))

          cpg.typeDecl.name("Client").member.name("clientCore").typeFullName.l shouldBe List(
            "com.example.clientcore.ClientCore"
          )

        case Failure(ex) =>
          fail(ex)
      }
    }
  }
}
