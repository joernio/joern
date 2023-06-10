package io.joern.rubysrc2cpg.passes

import better.files.File
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ConfigPassTest extends AnyWordSpec with Matchers {

  "ConfigPass for Gemfile files" should {

    "generate a ConfigFile accordingly" in {
      File.usingTemporaryDirectory(prefix = "rubysrc2cpgTest") { dir =>
        val gemFileName = dir / "Gemfile"
        val gemfileContents =
          """
            |source 'https://rubygems.org'
            |gem 'json'
            |""".stripMargin
        gemFileName.write(gemfileContents)

        val cpg = Cpg.emptyCpg
        new ConfigPass(cpg, dir.pathAsString).createAndApply()
        val List(configFile) = cpg.configFile.l
        configFile.name shouldBe "Gemfile"
        configFile.content shouldBe gemfileContents
      }
    }

    "ignore non-root Gemfile files" in {
      File.usingTemporaryDirectory(prefix = "rubysrc2cpgTest") { dir =>
        val subdir = dir / "subdir"
        subdir.createDirectory()
        val gemFileName     = subdir / "Gemfile"
        val gemFileContents = "# ignore me"
        gemFileName.write(gemFileContents)

        val cpg = Cpg.emptyCpg
        new ConfigPass(cpg, dir.pathAsString).createAndApply()
        cpg.configFile.size shouldBe 0
      }
    }
  }

  "ConfigPass for Gemfile.lock files" should {

    "generate a ConfigFile accordingly" in {
      File.usingTemporaryDirectory(prefix = "rubysrc2cpgTest") { dir =>
        val gemFileName = dir / "Gemfile.lock"
        val gemFileContents =
          """
            |GEM
            |  remote: https://rubygems.org/
            |  specs:
            |    CFPropertyList (3.0.1)
            |
            |PLATFORMS
            |  ruby
            |
            |BUNDLED WITH
            |   2.1.4
            |""".stripMargin
        gemFileName.write(gemFileContents)

        val cpg = Cpg.emptyCpg
        new ConfigPass(cpg, dir.pathAsString).createAndApply()
        val List(configFile) = cpg.configFile.l
        configFile.name shouldBe "Gemfile.lock"
        configFile.content shouldBe gemFileContents
      }
    }

    "ignore non-root Gemfile.lock files" in {
      File.usingTemporaryDirectory(prefix = "rubysrc2cpgTest") { dir =>
        val subdir = dir / "subdir"
        subdir.createDirectory()
        val gemFileName     = subdir / "Gemfile.lock"
        val gemFileContents = "# ignore me"

        gemFileName.write(gemFileContents)
        val cpg = Cpg.emptyCpg
        new ConfigPass(cpg, dir.pathAsString).createAndApply()
        cpg.configFile.size shouldBe 0
      }
    }
  }
}
