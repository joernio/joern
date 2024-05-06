package io.joern.kotlin2cpg.config

import io.joern.kotlin2cpg.Main
import io.joern.kotlin2cpg.Config

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import io.joern.x2cpg.X2Cpg
import org.scalatest.Inside

class ConfigTests extends AnyWordSpec with Matchers with Inside {

  "kotlin2cpg command line args should be parsed correctly" in {
    val parser = Main.cmdLineParser
    val args = Array(
      // Common args
      "INPUT",
      "--output",
      "OUTPUT",
      "--exclude",
      "1EXCLUDE_FILE,2EXCLUDE_FILE",
      "--exclude-regex",
      "EXCLUDE_REGEX",
      // Frontend-specific args
      "--classpath",
      "CLASSPATH",
      "--no-stdlib-jars",
      "--jar4import-url",
      "URL",
      "--download-dependencies",
      "--gradle-project-name",
      "GRADLE_PROJ_NAME",
      "--gradle-configuration-name",
      "GRADLE_CONF_NAME",
      "--include-java-sources"
    )

    def getSuffix(s: String, n: Int): String = s.takeRight(n)

    inside(X2Cpg.parseCommandLine(args, parser, Config())) { case Some(config) =>
      config.inputPath.endsWith("INPUT") shouldBe true
      config.outputPath shouldBe "OUTPUT"
      config.ignoredFiles.map(getSuffix(_, 13)).toSet shouldBe Set("1EXCLUDE_FILE", "2EXCLUDE_FILE")
      config.ignoredFilesRegex.toString shouldBe "EXCLUDE_REGEX"
      config.classpath shouldBe Set("CLASSPATH")
      config.withStdlibJarsInClassPath shouldBe false
      config.downloadDependencies shouldBe true
      config.gradleProjectName shouldBe Some("GRADLE_PROJ_NAME")
      config.gradleConfigurationName shouldBe Some("GRADLE_CONF_NAME")
      config.jar4importServiceUrl shouldBe Some("URL")
      config.includeJavaSourceFiles shouldBe true
    }
  }
}
