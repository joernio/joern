package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class DependencyTests extends RubyCode2CpgFixture {

  "parsing a Ruby Gems lock file" should {

    val cpg = code(
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
        |""".stripMargin,
      "Gemfile.lock"
    )

    "result in dependency nodes of the set packages" in {
      inside(cpg.dependency.l) {
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

}
