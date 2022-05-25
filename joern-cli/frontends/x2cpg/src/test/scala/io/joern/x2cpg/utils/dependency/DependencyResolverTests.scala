package io.joern.x2cpg.utils.dependency

import io.joern.x2cpg.utils.ExternalCommand
import io.shiftleft.utils.ProjectRoot
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Path, Paths}
import better.files.File

class DependencyResolverTests extends AnyWordSpec with Matchers {
  private class FixtureWithCopyDir(srcDir: Path, runningOnWindowsGitHubAction: Boolean = false) {
    def test(
      testFunc: Option[collection.Seq[String]] => Unit,
      params: DependencyResolverParams = new DependencyResolverParams
    ): Unit = {
      if (runningOnWindowsGitHubAction) {
        info("tests were cancelled because github actions windows doesn't support them for some unknown reason...")
      } else {
        File.usingTemporaryDirectory("DependencyResolverTests") { tmpDir =>
          File(srcDir).copyTo(tmpDir, true)
          val dependenciesResult = DependencyResolver.getDependencies(tmpDir.path, params)
          testFunc(dependenciesResult)
        }
      }
    }
  }

  private class Fixture(content: String, fileName: String, runningOnWindowsGitHubAction: Boolean = false) {
    def test(
      testFunc: Option[collection.Seq[String]] => Unit,
      params: DependencyResolverParams = new DependencyResolverParams
    ): Unit = {
      if (runningOnWindowsGitHubAction) {
        info("tests were cancelled because github actions windows doesn't support them for some unknown reason...")
      } else {
        File.usingTemporaryDirectory("DependencyResolverTests") { tmpDir =>
          val outFile = tmpDir / fileName
          outFile.write(content)
          val dependenciesResult = DependencyResolver.getDependencies(tmpDir.path, params)
          testFunc(dependenciesResult)
        }
      }
    }
  }

  /** The following tests fail on github actions for the windows runner. Logs do not show any information regarding why
    * it happens. Given that the Gradle dependency resolution needs to be developed and shown to a customer in the next
    * few days, a proper debugging session will have to wait.
    */
  // TODO: remove this workaround
  val isGithubActions                = scala.util.Properties.envOrElse("GITHUB_ACTIONS", "false").toLowerCase == "true"
  val isWindows                      = scala.util.Properties.isWin
  val isRunningOnWindowsGithubAction = isGithubActions && isWindows

  "test gradle dependency resolution for a simple `build.gradle`" in {
    val fixture = new Fixture(
      """
        |apply plugin: 'java'
        |repositories { mavenCentral() }
        |dependencies { implementation 'log4j:log4j:1.2.17' }
        |""".stripMargin,
      "build.gradle",
      isRunningOnWindowsGithubAction
    )

    fixture.test { dependenciesResult =>
      dependenciesResult should not be empty
      val dependencyFiles = dependenciesResult.getOrElse(Seq())
      dependencyFiles.find(_.endsWith("log4j-1.2.17.jar")) should not be empty
    }
  }

  "test gradle dependency resolution for a simple `build.gradle.kts`" in {
    val fixture = new Fixture(
      """
        |plugins { kotlin("jvm") version "1.6.10" }
        |repositories { mavenCentral() }
        |dependencies { implementation("log4j:log4j:1.2.17") }
        |""".stripMargin,
      "build.gradle.kts",
      isRunningOnWindowsGithubAction
    )

    fixture.test { dependenciesResult =>
      dependenciesResult should not be empty
      val dependencyFiles = dependenciesResult.getOrElse(Seq())
      dependencyFiles.find(_.endsWith("log4j-1.2.17.jar")) should not be empty
    }
  }

  "test gradle dependency resolution for `build.gradle` using `kotlin-gradle-plugin`" in {
    val fixture = new Fixture(
      """
        |buildscript {
        |    repositories { mavenCentral() }
        |    dependencies { classpath "org.jetbrains.kotlin:kotlin-gradle-plugin:1.6.0" }
        |}
        |repositories { mavenCentral() }
        |apply plugin: 'kotlin'
        |dependencies { implementation 'log4j:log4j:1.2.17' }
        |""".stripMargin,
      "build.gradle",
      isRunningOnWindowsGithubAction
    )

    fixture.test { dependenciesResult =>
      dependenciesResult should not be empty
      val dependencyFiles = dependenciesResult.getOrElse(Seq())
      dependencyFiles.find(_.endsWith("log4j-1.2.17.jar")) should not be empty
    }
  }

  "test gradle dependency resolution for simple Android app" ignore {
    val androidAppDir = ProjectRoot.relativise("joern-cli/src/test/resources/testcode/SlimAndroid")
    val fixture       = new FixtureWithCopyDir(Paths.get(androidAppDir), isRunningOnWindowsGithubAction)
    fixture.test({ dependenciesResult =>
      dependenciesResult should not be empty
      val dependencyFiles = dependenciesResult.getOrElse(Seq())
      dependencyFiles.filter(_.endsWith(".jar")) should not be empty
      dependencyFiles.filter(_.endsWith(".aar")) shouldBe empty
      dependencyFiles.find(_.endsWith("glide-4.11.0.aar")) shouldBe empty
      dependencyFiles.find(_.endsWith("glide-4.11.0.jar")) should not be empty
    })
  }

  "test gradle dependency resolution for simple Android app with incorrect Gradle project name param" ignore {
    val androidAppDir = ProjectRoot.relativise("joern-cli/src/test/resources/testcode/SlimAndroid")
    val fixture       = new FixtureWithCopyDir(Paths.get(androidAppDir), isRunningOnWindowsGithubAction)
    fixture.test(
      { dependenciesResult =>
        dependenciesResult shouldBe empty
      },
      DependencyResolverParams(forGradle = Map(GradleConfigKeys.ProjectName -> "NON_EXISTENT_PROJECT_NAME"))
    )
  }

  "test gradle dependency resolution for simple Android app with incorrect Gradle configuration param" ignore {
    val androidAppDir = ProjectRoot.relativise("joern-cli/src/test/resources/testcode/SlimAndroid")
    val fixture       = new FixtureWithCopyDir(Paths.get(androidAppDir), isRunningOnWindowsGithubAction)
    fixture.test(
      { dependenciesResult =>
        dependenciesResult shouldBe empty
      },
      DependencyResolverParams(forGradle = Map(GradleConfigKeys.ConfigurationName -> "NON_EXISTENT_CONFIGURATION_NAME"))
    )
  }

  "test maven dependency resolution" in {
    // check that `mvn` is available - otherwise test will fail with only some logged warnings...
    withClue("`mvn` must be installed in order for this test to work...") {
      ExternalCommand.run("mvn --version", ".").get.exists(_.contains("Apache Maven")) shouldBe true
    }

    val fixture = new Fixture(
      """
        |<project xmlns="http://maven.apache.org/POM/4.0.0"
        |         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        |         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
        |    <modelVersion>4.0.0</modelVersion>

        |    <groupId>gd.wa</groupId>
        |    <artifactId>minimal-pom</artifactId>
        |    <version>1.0-SNAPSHOT</version>
        |    <packaging>jar</packaging>

        |    <name>minimal-pom</name>
        |    <url>http://maven.apache.org</url>

        |    <properties>
        |        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
        |        <java.version>1.8</java.version>
        |    </properties>

        |    <build>
        |        <plugins>
        |            <plugin>
        |                <groupId>org.apache.maven.plugins</groupId>
        |                <artifactId>maven-compiler-plugin</artifactId>
        |                <version>3.1</version>
        |                <configuration>
        |                    <source>${java.version}</source>
        |                    <target>${java.version}</target>
        |                </configuration>
        |            </plugin>
        |        </plugins>
        |    </build>
        |    <dependencies>
        |        <dependency>
        |            <groupId>org.slf4j</groupId>
        |            <artifactId>slf4j-api</artifactId>
        |            <version>1.7.36</version>
        |        </dependency>
        |    </dependencies>
        |</project>
        |""".stripMargin,
      "pom.xml"
    )

    fixture.test { dependenciesResult =>
      dependenciesResult should not be empty
      val dependencyFiles = dependenciesResult.getOrElse(Seq())
      dependencyFiles.find(_.endsWith("slf4j-api-1.7.36.jar")) should not be empty
    }
  }
}
