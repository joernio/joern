package io.joern.rubysrc2cpg.io

import io.joern.rubysrc2cpg.{Config, RubySrc2Cpg}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}

class ExcludeRegexTests extends AnyWordSpec with Matchers {

  /** Relative paths use the separator of the operating system, so regexes must accept both `/` and `\`. */
  private val sep = "[/\\\\]"

  private def parsedFiles(regex: String): Set[String] = {
    FileUtil.usingTemporaryDirectory("rubysrc2cpgExclude") { dir =>
      Files.createDirectories(dir / "pkg" / "a")
      Files.writeString(dir / "main.rb", "puts 1\n")
      Files.writeString(dir / "pkg" / "a" / "a.rb", "puts 2\n")
      val frontend = new RubySrc2Cpg()
      try {
        val cpg = frontend.createCpg(Config().withInputPath(dir.toString).withIgnoredFilesRegex(regex)).get
        try {
          cpg.file.name.filter(_.endsWith(".rb")).toSet
        } finally {
          cpg.close()
        }
      } finally {
        frontend.close()
      }
    }
  }

  private val mainFile = "main.rb"
  private val libFile  = Paths.get("pkg", "a", "a.rb").toString

  "--exclude-regex" should {

    "exclude a file whose relative path fully matches" in {
      parsedFiles("main\\.rb") shouldBe Set(libFile)
    }

    "exclude all files below a directory whose relative path fully matches" in {
      parsedFiles(s"pkg${sep}a") shouldBe Set(mainFile)
    }

    "not exclude anything if it only matches the name of the generated JSON files" in {
      parsedFiles(".*json.*") shouldBe Set(mainFile, libFile)
      parsedFiles(".*\\.rb\\.json") shouldBe Set(mainFile, libFile)
    }
  }
}
