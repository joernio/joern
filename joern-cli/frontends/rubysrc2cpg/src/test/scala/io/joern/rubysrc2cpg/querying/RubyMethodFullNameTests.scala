package io.joern.rubysrc2cpg.querying

import better.files.File
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language._
import org.scalatest.BeforeAndAfterAll

class RubyMethodFullNameTests extends RubyCode2CpgFixture with BeforeAndAfterAll {

  var tempDir: File = _

  override def afterAll(): Unit = {
    super.beforeAll()
    tempDir.delete()
  }

  "Code for method full name when method present in same file for simple class" should {
    val cpg = code(
      """
        |class Hello
        | def printValue(value)
        |   puts value
        | end
        |end
        |
        |v = Hello.new
        |puts v.printValue("any")
        |""".stripMargin,
      "main.rb"
    )

    "recognise all call node" in {
      cpg.call.name("printValue").l.size shouldBe 1
    }

    "recognise methodFullName for call node" in {
      cpg.call.name("printValue").head.methodFullName should equal("Hello.printValue:<unresolvedSignature>")
    }
  }

  "Code for method full name when method present in same file for nested class" should {
    val cpg = code("""
        |module Help
        | class InnerHelp
        |   def printValue(value)
        |     puts value
        |   end
        | end
        |end
        |
        |val v = Help::InnerHelp.new
        |puts v.printValue("any")
        |""".stripMargin)

    "recognise all call node" in {
      cpg.call.name("printValue").l.size shouldBe 1
    }

    "recognise methodFullName for call node" in {
      cpg.call.name("printValue").head.methodFullName should equal("InnerHelp.Help.printValue:<unresolvedSignature>")
    }
  }

  "Code for method full name when method present in another file" should {
    tempDir = File.newTemporaryDirectory()
    val tempPath = tempDir.toString()
    val cpg = code(
      s"""
        |require "$tempPath/help.rb"
        |
        |f = Help::Inner.new
        |v.getValue("ggg")
        |""".stripMargin,
      "main.rb"
    )

    val helpFile =
      s"""
        |module Help
        |    class Inner
        |        def getValue(value)
        |            puts value
        |        end
        |
        |        def printValue(value)
        |            puts "ana"
        |        end
        |    end
        |end
        |""".stripMargin

    (tempDir / "help.rb").write(helpFile)

    "recognise call node" in {
      cpg.call.name("getValue").l.size shouldBe 1
    }

    "recognise methodFullName for call Node" in {
      cpg.call.name("getValue").head.methodFullName should equal("Inner.Help.getValue:<unresolvedSignature>")
    }
  }

  "Code for method full name when method present in module" should {
    val cpg = code(
      """
        |require "dummy_logger"
        |
        |v = Main_module::Main_outer_class.new
        |v.first_fun("value")
        |""".stripMargin,
      "main.rb"
    )
      .moreCode(
        """
          |source 'https://rubygems.org'
          |gem 'dummy_logger'
          |""".stripMargin,
        "Gemfile"
      )

    "recognise call node" in {
      cpg.call.name("first_fun").l.size shouldBe 1
    }

    "recognise methodFullName for call Node" in {
      cpg.call.name("first_fun").head.methodFullName should equal(
        "dummy_logger.Main_outer_class.Main_module.first_fun:<unresolvedSignature>"
      )
    }
  }
}
