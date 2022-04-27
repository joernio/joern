package io.joern.x2cpg.utils.dependency

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Path}
import java.util.Comparator

class DependencyResolverTests extends AnyWordSpec with Matchers {
  private class Fixture(content: String, fileName: String) {
    def test(testFunc: collection.Seq[String] => Unit): Unit = {
      val tmpDir = Files.createTempDirectory("DependencyResolverTests")
      try {
        val file = tmpDir.resolve(fileName)
        Files.write(file, content.getBytes)

        val dependenciesFiles = DependencyResolver.getDependencies(tmpDir)
        testFunc(dependenciesFiles)
      } finally {
        Files
          .walk(tmpDir)
          .sorted(Comparator.reverseOrder[Path]())
          .forEach(Files.delete(_))
      }
    }

  }

  "test maven dependency resolution" in {
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

    fixture.test { dependenciesFiles =>
      dependenciesFiles.find(_.endsWith("slf4j-api-1.7.36.jar")) should not be empty
    }
  }
}
