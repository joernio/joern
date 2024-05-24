package io.joern.rubysrc2cpg.passes

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class ImportDependencyLinkerPassTests
    extends RubyCode2CpgFixture(withPostProcessing = true, downloadDependencies = true) {
  "Code for method full name when method present in module" should {
    val cpg = code(
      """
        |require "dummy_logger"
        |
        |v = Main_module::Main_outer_class.new
        |v.first_fun("value")
        |
        |g = Help.new
        |g.help_print()
        |
        |""".stripMargin,
      "main.rb"
    )
      .moreCode(
        """
        |source 'https://rubygems.org'
        |gem 'dummy_logger'
        |gem 'logger'
        |""".stripMargin,
        "Gemfile"
      )

    "Create a File node for dummy_logger dependency" in {
      cpg.file.name("dummy_logger.rb").l.size shouldBe 1
    }

    "Not create a file node for logger dependency" in {
      cpg.file.name("logger.rb").l.size shouldBe 0
    }
  }
}
