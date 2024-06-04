package io.joern.rubysrc2cpg.deprecated.passes

import better.files.File
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ConfigFileCreationPassTest extends RubyCode2CpgFixture(useDeprecatedFrontend = true) {

  "ConfigFileCreationPass for Gemfile files" should {

    "generate a ConfigFile accordingly" in {
      val gemFileContents =
        """
          |source 'https://rubygems.org'
          |gem 'json'
          |""".stripMargin
      val cpg = code(gemFileContents, "Gemfile")

      val List(configFile) = cpg.configFile.l
      configFile.name shouldBe "Gemfile"
      configFile.content shouldBe gemFileContents
    }

    "ignore non-root Gemfile files" in {
      val cpg = code("# ignore me", Seq("subdir", "Gemfile").mkString(java.io.File.pathSeparator))
      cpg.configFile.size shouldBe 0
    }
  }

  "ConfigFileCreationPass for Gemfile.lock files" should {

    "generate a ConfigFile accordingly" in {
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
      val cpg              = code(gemFileContents, "Gemfile.lock")
      val List(configFile) = cpg.configFile.l
      configFile.name shouldBe "Gemfile.lock"
      configFile.content shouldBe gemFileContents
    }

    "ignore non-root Gemfile.lock files" in {
      val cpg = code("# ignore me", Seq("subdir", "Gemfile.lock").mkString(java.io.File.pathSeparator))
      cpg.configFile.size shouldBe 0
    }
  }
}
