package io.joern.jimple2cpg.config

import io.joern.jimple2cpg.Main
import io.joern.jimple2cpg.Config

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import io.joern.x2cpg.X2Cpg
import org.scalatest.Inside

class ConfigTests extends AnyWordSpec with Matchers with Inside {

  "jimple2cpg command line args should be parsed correctly" in {
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
      "--android",
      "ANDROID"
    )

    def getSuffix(s: String, n: Int): String = {
      s.reverse.take(n).reverse
    }

    inside(X2Cpg.parseCommandLine(args, parser, Config())) { case Some(config) =>
      config.inputPath.endsWith("INPUT") shouldBe true
      config.outputPath shouldBe "OUTPUT"
      config.ignoredFiles.map(getSuffix(_, 13)).toSet shouldBe Set("1EXCLUDE_FILE", "2EXCLUDE_FILE")
      config.ignoredFilesRegex.toString shouldBe "EXCLUDE_REGEX"
      config.android shouldBe Some("ANDROID")
    }
  }
}
