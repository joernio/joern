package io.joern.pysrc2cpg.config

import io.joern.pysrc2cpg.NewMain
import io.joern.pysrc2cpg.Py2CpgOnFileSystemConfig

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import io.joern.x2cpg.X2Cpg
import org.scalatest.Inside

class ConfigTests extends AnyWordSpec with Matchers with Inside {

  "pysrc2cpg command line args should be parsed correctly" in {
    val parser = NewMain.cmdLineParser
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
      "--venvDir",
      "VENV_DIR",
      "--ignoreVenvDir",
      "false",
      "--no-dummyTypes"
    )

    def getSuffix(s: String, n: Int): String = {
      s.takeRight(n)
    }

    inside(X2Cpg.parseCommandLine(args, parser, Py2CpgOnFileSystemConfig())) { case Some(config) =>
      config.inputPath.endsWith("INPUT") shouldBe true
      config.outputPath shouldBe "OUTPUT"
      config.ignoredFiles.map(getSuffix(_, 13)).toSet shouldBe Set("1EXCLUDE_FILE", "2EXCLUDE_FILE")
      config.ignoredFilesRegex.toString shouldBe "EXCLUDE_REGEX"
      config.venvDir.exists(_.endsWith("VENV_DIR")) shouldBe true
      config.ignoreVenvDir shouldBe false
      config.disableDummyTypes shouldBe true
    }
  }
}
