package io.joern.x2cpg.utils.dependency

import io.joern.x2cpg.utils.ExternalCommand
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import better.files.File
import scala.annotation.nowarn

class DependencyResolverTests extends AnyWordSpec with Matchers {

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
          outFile.createIfNotExists(createParents = true)
          outFile.write(content)
          val dependenciesResult = DependencyResolver.getDependencies(tmpDir.path, params)
          testFunc(dependenciesResult)
        }
      }
    }
  }

  "test maven dependency resolution" ignore {
    // check that `mvn` is available - otherwise test will fail with only some logged warnings...
    withClue("`mvn` must be installed in order for this test to work...") {
      ExternalCommand.run(Seq("mvn", "--version"), ".").successOption.exists(_.contains("Apache Maven")) shouldBe true
    }

    @nowarn // otherwise scalac warns that this might be an interpolated expression
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
      "subdir/pom.xml"
    )

    fixture.test { dependenciesResult =>
      dependenciesResult should not be empty
      val dependencyFiles = dependenciesResult.getOrElse(Seq())
      dependencyFiles.find(_.endsWith("slf4j-api-1.7.36.jar")) should not be empty
    }
  }
}
