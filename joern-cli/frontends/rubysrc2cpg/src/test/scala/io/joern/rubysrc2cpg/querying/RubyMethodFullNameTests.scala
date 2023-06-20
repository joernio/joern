package io.joern.rubysrc2cpg.querying

import better.files.File
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language._
import org.scalatest.BeforeAndAfterAll

class RubyMethodFullNameTests extends RubyCode2CpgFixture(true) with BeforeAndAfterAll {

  var tempDir: File = _

  override def afterAll(): Unit = {
    super.beforeAll()
    tempDir.delete()
  }

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
          |
          |""".stripMargin,
        "Gemfile"
      )

    "recognise call node" in {
      cpg.call.name("first_fun").l.size shouldBe 1
    }

    "recognise import node" in {
      cpg.imports.code(".*dummy_logger.*").l.size shouldBe 1
    }

    "recognise methodFullName for call Node" in {
      cpg.call.name("first_fun").head.methodFullName should equal(
        "dummy_logger.Main_module.Main_outer_class.first_fun:<unresolvedSignature>"
      )

      cpg.call.name("help_print").head.methodFullName should equal("dummy_logger.Help.help_print:<unresolvedSignature>")
    }
  }

  "Code for method full name when method present in other file" should {
    val cpg = code(
      """
        |require_relative "util/help.rb"
        |
        |v = Outer.new
        |v.printValue()
        |
        |""".stripMargin,
      "main.rb"
    )
      .moreCode(
        """
          |class Outer
          | def printValue()
          |   puts "print"
          | end
          |end
          |""".stripMargin,
        Seq("util", "help.rb").mkString(java.io.File.separator)
      )

    "recognise call node" in {
      cpg.call.name("printValue").l.size shouldBe 1
    }

    "recognise import node" in {
      cpg.imports.code(".*util/help.rb*.").l.size shouldBe 1
    }

    "recognise method full name for call node" in {
      cpg.call.name("printValue").head.methodFullName contains ("util/help.rb.Outer.printValue:<unresolvedSignature>")
    }
  }
}
