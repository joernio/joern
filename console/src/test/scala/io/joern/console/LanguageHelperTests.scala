package io.joern.console

import better.files.Dsl._
import better.files._
import io.shiftleft.codepropertygraph.generated.Languages
import io.joern.console.cpgcreation.LlvmCpgGenerator
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class LanguageHelperTests extends AnyWordSpec with Matchers {

  import io.joern.console.cpgcreation.guessLanguage

  "LanguageHelper.guessLanguage" should {

    "guess `Java` for .jars/wars/ears" in {
      guessLanguage("foo.jar") shouldBe Some(Languages.JAVA)
      guessLanguage("foo.war") shouldBe Some(Languages.JAVA)
      guessLanguage("foo.ear") shouldBe Some(Languages.JAVA)
    }

    "guess `C#` for .csproj" in {
      guessLanguage("foo.csproj") shouldBe Some(Languages.CSHARP)
    }

    "guess `Go` for a .go file" in {
      guessLanguage("foo.go") shouldBe Some(Languages.GOLANG)
    }

    "guess `Go` for a directory containing `Gopkg.lock`" in {
      File.usingTemporaryDirectory("oculartests") { tmpDir =>
        val subdir = mkdir(tmpDir / "subdir")
        touch(subdir / "Gopkg.lock")
        guessLanguage(tmpDir.pathAsString) shouldBe Some(Languages.GOLANG)
      }
    }

    "guess `Go` for a directory containing `Gopkg.toml`" in {
      File.usingTemporaryDirectory("oculartests") { tmpDir =>
        val subdir = mkdir(tmpDir / "subdir")
        touch(subdir / "Gopkg.toml")
        guessLanguage(tmpDir.pathAsString) shouldBe Some(Languages.GOLANG)
      }
    }

    "guess `Javascript` for a directory containing `package.json`" in {
      File.usingTemporaryDirectory("oculartests") { tmpDir =>
        val subdir = mkdir(tmpDir / "subdir")
        touch(subdir / "package.json")
        guessLanguage(tmpDir.pathAsString) shouldBe Some(Languages.JAVASCRIPT)
      }
    }

    "guess `C` for a directory containing .ll (LLVM) file" in {
      File.usingTemporaryDirectory("oculartests") { tmpDir =>
        val subdir = mkdir(tmpDir / "subdir")
        touch(subdir / "foobar.ll")
        guessLanguage(tmpDir.pathAsString) shouldBe Some(Languages.LLVM)
      }
    }

    "guess `C` for a directory that does not contain any special file" in {
      File.usingTemporaryDirectory("oculartests") { tmpDir =>
        guessLanguage(tmpDir.pathAsString) shouldBe Some(Languages.C)
      }
    }

  }

  "LanguageHelper.cpgGeneratorForLanguage" should {

    "select LLVM frontend for directories containing ll files" in {
      val frontend = io.joern.console.cpgcreation.cpgGeneratorForLanguage(
        Languages.LLVM,
        FrontendConfig(),
        File(".").path,
        Nil
      )
      frontend.get.isInstanceOf[LlvmCpgGenerator] shouldBe true
    }
  }

}
