package io.joern.go2cpg.config

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.joern.gosrc2cpg.Config
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*

import java.io.File

class ExcludeRegexTests extends GoCodeToCpgSuite {

  private val mainFile = "main.go"
  private val libFile  = Seq("pkg", "a", "a.go").mkString(File.separator)

  private def cpgWithExcludeRegex(regex: String): Cpg = {
    code("module example.com/m\ngo 1.18\n", "go.mod")
      .moreCode("package main\nfunc main() {}\n", mainFile)
      .moreCode("package a\nfunc Foo() int { return 1 }\n", libFile)
      .withConfig(Config().withIgnoredFilesRegex(regex))
  }

  private def userMethods(cpg: Cpg): Set[String] = cpg.method.isExternal(false).name("main|Foo").name.toSet

  "A regex that fully matches the relative path of a file" should {
    "exclude that file only" in {
      userMethods(cpgWithExcludeRegex("main\\.go")) shouldBe Set("Foo")
    }

    "exclude all files below a matching directory" in {
      userMethods(cpgWithExcludeRegex("pkg/a/.*")) shouldBe Set("main")
    }

    "support lookaheads that are not understood by goastgen" in {
      userMethods(cpgWithExcludeRegex("(?!pkg/a/.*$).*\\.go")) shouldBe Set("Foo")
    }
  }

  "A regex that only matches part of the relative path of a file" should {
    "not exclude anything" in {
      userMethods(cpgWithExcludeRegex("ain")) shouldBe Set("main", "Foo")
      userMethods(cpgWithExcludeRegex("kg/a")) shouldBe Set("main", "Foo")
    }
  }

  "A regex that only matches the absolute path of a file" should {
    "not exclude anything" in {
      userMethods(cpgWithExcludeRegex(".*/main\\.go")) shouldBe Set("main", "Foo")
    }
  }
}
