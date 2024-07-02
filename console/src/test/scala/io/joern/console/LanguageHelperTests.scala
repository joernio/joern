package io.joern.console

import better.files.Dsl.*
import better.files.*
import io.shiftleft.codepropertygraph.generated.Languages
import io.joern.console.cpgcreation.{guessLanguage, LlvmCpgGenerator}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class LanguageHelperTests extends AnyWordSpec with Matchers {

  "LanguageHelper.guessLanguage" should {

    "guess `Java` for .jars/wars/ears" in {
      guessLanguage("foo.jar") shouldBe Some(Languages.JAVA)
      guessLanguage("foo.war") shouldBe Some(Languages.JAVA)
      guessLanguage("foo.ear") shouldBe Some(Languages.JAVA)
    }

    "guess `C#` for .csproj" in {
      guessLanguage("foo.csproj") shouldBe Some(Languages.CSHARPSRC)
    }

    "guess `Go` for a .go file" in {
      guessLanguage("foo.go") shouldBe Some(Languages.GOLANG)
    }

    "guess `JavaSrc` for a directory containing `.java`" in {
      File.usingTemporaryDirectory("oculartests") { tmpDir =>
        val subdir = mkdir(tmpDir / "subdir")
        touch(subdir / "ServiceIdentifierComposerVisitorBasedStrategy.java")
        guessLanguage(tmpDir.pathAsString) shouldBe Some(Languages.JAVASRC)
      }
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
        guessLanguage(tmpDir.pathAsString) shouldBe Some(Languages.JSSRC)
      }
    }

    "guess `Swift` for a directory containing `.swift`" in {
      File.usingTemporaryDirectory("oculartests") { tmpDir =>
        val subdir = mkdir(tmpDir / "subdir")
        touch(subdir / "main.swift")
        guessLanguage(tmpDir.pathAsString) shouldBe Some(Languages.SWIFTSRC)
      }
    }

    "guess `C` for a directory containing .ll (LLVM) file" in {
      File.usingTemporaryDirectory("oculartests") { tmpDir =>
        val subdir = mkdir(tmpDir / "subdir")
        touch(subdir / "foobar.ll")
        guessLanguage(tmpDir.pathAsString) shouldBe Some(Languages.LLVM)
      }
    }

    "guess the language with the largest number of files" in {
      File.usingTemporaryDirectory("oculartests") { tmpDir =>
        val subdir = mkdir(tmpDir / "subdir")
        touch(subdir / "source.c")
        touch(subdir / "source.java")
        touch(subdir / "source.py")
        touch(subdir / "source.js")
        touch(subdir / "package.json") // also counts towards javascript
        touch(subdir / "source.py")
        guessLanguage(tmpDir.pathAsString) shouldBe Some(Languages.JSSRC)
      }
    }

    "not find anything for an empty directory" in {
      File.usingTemporaryDirectory("oculartests") { tmpDir =>
        guessLanguage(tmpDir.pathAsString) shouldBe None
      }
    }

  }

  "LanguageHelper.cpgGeneratorForLanguage" should {

    "select LLVM frontend for directories containing ll files" in {
      val frontend =
        io.joern.console.cpgcreation.cpgGeneratorForLanguage(Languages.LLVM, FrontendConfig(), File(".").path, Nil)
      frontend.get.isInstanceOf[LlvmCpgGenerator] shouldBe true
    }
  }

}
