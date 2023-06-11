package io.joern.rubysrc2cpg.passes

import better.files.File
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DependenciesPassTest extends AnyWordSpec with Matchers {

  "DependenciesPass on Gemfile files" should {

    "generate dependency nodes correctly when no gem version is specified" in {
      File.usingTemporaryDirectory(prefix = "rubysrc2cpgTest") { dir =>
        val gemFileName = dir / "Gemfile"
        val gemFileContents =
          """
            |source 'https://rubygems.org'
            |gem 'json'
            |""".stripMargin
        gemFileName.write(gemFileContents)
        val cpg = Cpg.emptyCpg
        new ConfigPass(cpg, dir.pathAsString).createAndApply()
        new DependenciesPass(cpg).createAndApply()

        val List(depA) = cpg.dependency.l
        depA.name shouldBe "json"
      }
    }

    "generate dependency nodes correctly when a single version constraint is specified" in {
      File.usingTemporaryDirectory(prefix = "rubysrc2cpgTest") { dir =>
        val gemFileName = dir / "Gemfile"
        val gemFileContents =
          """
            |source 'https://rubygems.org'
            |gem 'puma', '~> 1.2'
            |""".stripMargin
        gemFileName.write(gemFileContents)
        val cpg = Cpg.emptyCpg
        new ConfigPass(cpg, dir.pathAsString).createAndApply()
        new DependenciesPass(cpg).createAndApply()

        val List(depA) = cpg.dependency.l
        depA.name shouldBe "puma"
        depA.version shouldBe "~> 1.2"
      }
    }
  }
}
