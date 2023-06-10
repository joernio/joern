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
        val gemfile = dir / "Gemfile"

        val gemfileContents =
          """
            |source 'https://rubygems.org'
            |gem 'json'
            |""".stripMargin

        gemfile.write(gemfileContents)

        val cpg = Cpg.emptyCpg
        new ConfigPass(cpg, dir.pathAsString).createAndApply()

        val List(configGemfile) = cpg.configFile.l
        configGemfile.name shouldBe "Gemfile"
        configGemfile.content shouldBe gemfileContents
      }
    }

    "ignore non-root Gemfile files" in {
      File.usingTemporaryDirectory(prefix = "rubysrc2cpgTest") { dir =>
        val subdir = dir / "subdir"
        subdir.createDirectory()

        val gemfile = subdir / "Gemfile"
        val gemfileContents = "# ignore me"

        gemfile.write(gemfileContents)

        val cpg = Cpg.emptyCpg
        new ConfigPass(cpg, dir.pathAsString).createAndApply()

        cpg.configFile.l shouldBe Seq()
      }
    }
  }

  "ConfigPass for Gemfile.lock files" should {

    "generate a ConfigFile accordingly" in {
      File.usingTemporaryDirectory(prefix = "rubysrc2cpgTest") { dir =>
        val gemfilelock = dir / "Gemfile.lock"

        val gemfilelockContents =
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

        gemfilelock.write(gemfilelockContents)

        val cpg = Cpg.emptyCpg
        new ConfigPass(cpg, dir.pathAsString).createAndApply()

        val List(configGemfilelock) = cpg.configFile.l
        configGemfilelock.name shouldBe "Gemfile.lock"
        configGemfilelock.content shouldBe gemfilelockContents
      }
    }

    "ignore non-root Gemfile.lock files" in {
      File.usingTemporaryDirectory(prefix = "rubysrc2cpgTest") { dir =>
        val subdir = dir / "subdir"
        subdir.createDirectory()

        val gemfilelock = subdir / "Gemfile.lock"
        val gemfilelockContents = "# ignore me"

        gemfilelock.write(gemfilelockContents)

        val cpg = Cpg.emptyCpg
        new ConfigPass(cpg, dir.pathAsString).createAndApply()

        cpg.configFile.l shouldBe Seq()
      }
    }
  }
}
