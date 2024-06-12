package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.joern.x2cpg.Defines
import io.joern.rubysrc2cpg.passes.Defines as RubyDefines
import io.shiftleft.codepropertygraph.generated.nodes.{Block, Identifier}
import io.shiftleft.semanticcpg.language.*

class DependencyTests extends RubyCode2CpgFixture {

  "parsing a Ruby Gems lock file" should {

    val cpg = code(DependencyTests.GEMFILELOCK, "Gemfile.lock")

    "result in dependency nodes of the set packages" in {
      inside(cpg.dependency.nameNot(RubyDefines.Resolver).l) {
        case aruba :: bcrypt :: betterErrors :: Nil =>
          aruba.name shouldBe "aruba"
          aruba.version shouldBe "0.14.12"

          bcrypt.name shouldBe "bcrypt"
          bcrypt.version shouldBe "3.1.13"

          betterErrors.name shouldBe "better_errors"
          betterErrors.version shouldBe "2.5.1"

        case xs => fail(s"Expected exactly three dependencies, instead got [${xs.name.mkString(",")}]")
      }
    }

  }

  "parsing a Ruby Gems file" should {

    val cpg = code(DependencyTests.GEMFILE, "Gemfile")

    "result in dependency nodes of the set packages" in {
      inside(cpg.dependency.nameNot(RubyDefines.Resolver).l) {
        case aruba :: bcrypt :: coffeeRails :: Nil =>
          aruba.name shouldBe "aruba"
          aruba.version shouldBe "2.5.1"

          bcrypt.name shouldBe "bcrypt"
          bcrypt.version shouldBe ""

          coffeeRails.name shouldBe "coffee-rails"
          coffeeRails.version shouldBe ""

        case xs => fail(s"Expected exactly three dependencies, instead got [${xs.name.mkString(",")}]")
      }
    }

  }

  "a Gems lock file" should {

    val cpg = code(DependencyTests.GEMFILE, "Gemfile").moreCode(DependencyTests.GEMFILELOCK, "Gemfile.lock")

    "be preferred over a normal Gemfile" in {
      // Our Gemfile.lock specifies exact versions whereas the Gemfile does not
      cpg.dependency.nameNot(RubyDefines.Resolver).forall(d => !d.version.isBlank) shouldBe true
    }

  }
}

class DownloadDependencyTest extends RubyCode2CpgFixture(downloadDependencies = true) {

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

    "recognize the full method name of the imported Main_outer_class's constructor" in {
      inside(cpg.assignment.where(_.target.isIdentifier.name("v")).argument.l) {
        case (v: Identifier) :: (block: Block) :: Nil =>
          v.dynamicTypeHintFullName should contain("dummy_logger.Main_module.Main_outer_class")

          inside(block.astChildren.isCall.nameExact("new").headOption) {
            case Some(constructorCall) =>
              constructorCall.methodFullName shouldBe s"dummy_logger.Main_module.Main_outer_class:${RubyDefines.Initialize}"
            case None => fail(s"Expected constructor call, did not find one")
          }
        case xs => fail(s"Expected two arguments under the constructor assignment, got [${xs.code.mkString(", ")}]")
      }
    }

    "recognize the full method name of the imported Help's constructor" in {
      inside(cpg.assignment.where(_.target.isIdentifier.name("g")).argument.l) {
        case (g: Identifier) :: (block: Block) :: Nil =>
          g.dynamicTypeHintFullName should contain("dummy_logger.Help")

          inside(block.astChildren.isCall.name("new").headOption) {
            case Some(constructorCall) =>
              constructorCall.methodFullName shouldBe s"dummy_logger.Help:${RubyDefines.Initialize}"
            case None => fail(s"Expected constructor call, did not find one")
          }
        case xs => fail(s"Expected two arguments under the constructor assignment, got [${xs.code.mkString(", ")}]")
      }
    }

    // TODO: This requires type propagation
    "recognise methodFullName for `first_fun`" ignore {
      cpg.call.name("first_fun").head.methodFullName should equal(
        "dummy_logger::program:Main_module:Main_outer_class:first_fun"
      )
      cpg.call
        .name("help_print")
        .head
        .methodFullName shouldBe "dummy_logger::program:Help:help_print"
    }
  }

  "parsing logger repo" should {
    val cpg = code(
      """
        |require "logger"
        |""".stripMargin,
      "main.rb"
    )
      .moreCode(
        """
          |source 'https://rubygems.org'
          |gem 'logger'
          |
          |""".stripMargin,
        "Gemfile"
      )

    "Not throw any exceptions" in {}
  }

}

object DependencyTests {
  private val GEMFILELOCK =
    """
      |GEM
      |  remote: https://rubygems.org/
      |  specs:
      |    aruba (0.14.12)
      |      childprocess (>= 0.6.3, < 4.0.0)
      |      contracts (~> 0.9)
      |      cucumber (>= 1.3.19)
      |      ffi (~> 1.9)
      |      rspec-expectations (>= 2.99)
      |      thor (~> 0.19)
      |    bcrypt (3.1.13)
      |    better_errors (2.5.1)
      |      coderay (>= 1.0.0)
      |      erubi (>= 1.0.0)
      |      rack (>= 0.9.0)
      |
      |PLATFORMS
      |  ruby
      |
      |DEPENDENCIES
      |  aruba
      |  bcrypt
      |  better_errors
      |
      |RUBY VERSION
      |   ruby 2.6.5p114
      |
      |BUNDLED WITH
      |   1.17.3
      |
      |""".stripMargin

  private val GEMFILE =
    """
        |# frozen_string_literal: true
        |source "https://rubygems.org"
        |
        |ruby "2.6.5"
        |
        |gem "aruba", '2.5.1'
        |gem "bcrypt"
        |gem "coffee-rails"
        |""".stripMargin
}
