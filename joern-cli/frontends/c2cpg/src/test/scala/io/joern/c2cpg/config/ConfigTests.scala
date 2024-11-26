package io.joern.c2cpg.config

import io.joern.c2cpg.Main
import io.joern.c2cpg.Config

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import io.joern.x2cpg.X2Cpg
import org.scalatest.Inside

class ConfigTests extends AnyWordSpec with Matchers with Inside {

  "c2cpg command line args should be parsed correctly" in {
    val parser = Main.cmdLineParser
    val args = Array(
      // Common args
      "INPUT",
      "--output",
      "OUTPUT",
      "--exclude",
      "1EXCLUDE_FILE",
      "--exclude",
      "2EXCLUDE_FILE",
      "--exclude-regex",
      "EXCLUDE_REGEX",
      // Frontend-specific args
      "--include-comments",
      "--log-problems",
      "--log-preprocessor",
      "--print-ifdef-only",
      "--include",
      "INCLUDE_PATH",
      "--with-include-auto-discovery",
      "--define",
      "DEFINED_NAME"
    )

    def getSuffix(s: String, n: Int): String = {
      s.takeRight(n)
    }

    inside(X2Cpg.parseCommandLine(args, parser, Config())) { case Some(config) =>
      config.inputPath.endsWith("INPUT") shouldBe true
      config.outputPath shouldBe "OUTPUT"
      config.ignoredFiles.map(getSuffix(_, 13)).toSet shouldBe Set("1EXCLUDE_FILE", "2EXCLUDE_FILE")
      config.ignoredFilesRegex.toString shouldBe "EXCLUDE_REGEX"
      config.includeComments shouldBe true
      config.logProblems shouldBe true
      config.logPreprocessor shouldBe true
      config.printIfDefsOnly shouldBe true
      config.includePaths shouldBe Set("INCLUDE_PATH")
      config.includePathsAutoDiscovery shouldBe true
      config.defines shouldBe Set("DEFINED_NAME")
    }
  }
}
