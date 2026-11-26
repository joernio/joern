package io.joern.csharpsrc2cpg.io

import io.joern.csharpsrc2cpg.Config
import io.joern.csharpsrc2cpg.utils.DotNetAstGenRunner
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Path, Paths}

class ExcludeTests extends AnyWordSpec with Matchers with TableDrivenPropertyChecks with BeforeAndAfterAll {

  /** Relative paths use the separator of the operating system, so regexes must accept both `/` and `\`. */
  private val sep = "[/\\\\]"

  private val testFiles = Seq("a.cs", "b.cs", "sub/c.cs", "sub/d.cs", "other/e.cs")

  /** Escapes a path for a regex that is also passed to the native AST generator. `Pattern.quote` is not an option, as
    * `\Q...\E` is only understood by Java.
    */
  private def escaped(path: Path): String = path.toString.replaceAll("([^A-Za-z0-9_/])", "\\\\$1")

  private val projectUnderTest: Path = {
    val dir = Files.createTempDirectory("csharpsrc2cpgTestsExclude")
    testFiles.foreach { testFile =>
      val file = dir / testFile
      file.createWithParentsIfNotExists(createParents = true)
      Files.writeString(file, "class " + testFile.replaceAll("\\W", "_") + " { }")
    }
    dir
  }

  override def afterAll(): Unit = FileUtil.delete(projectUnderTest, swallowIoExceptions = true)

  private def parsedFiles(excludeRegex: String): Set[String] = {
    FileUtil.usingTemporaryDirectory("csharpsrc2cpgTests") { tmpDir =>
      val config = Config().withInputPath(projectUnderTest.toString).withOutputPath(tmpDir.toString)
      val result = new DotNetAstGenRunner(config.withIgnoredFilesRegex(excludeRegex)).execute(tmpDir)
      // the AST generator replaces the extension: `sub/c.cs` becomes `sub/c.json`
      result.parsedFiles.map(file => tmpDir.relativize(Paths.get(file)).toString.stripSuffix(".json") + ".cs").toSet
    }
  }

  private def relative(files: String*): Set[String] = files.map(_.replace("/", java.io.File.separator)).toSet

  "Using --exclude-regex" should {

    val testInput = Table(
      ("statement", "excludeRegex", "expectedResult"),
      ("exclude nothing by default", "", relative("a.cs", "b.cs", "sub/c.cs", "sub/d.cs", "other/e.cs")),
      ("exclude a file", ".*a\\.cs", relative("b.cs", "sub/c.cs", "sub/d.cs", "other/e.cs")),
      (
        "exclude a folder with a regex that is relative to the input directory",
        s"sub$sep.*",
        relative("a.cs", "b.cs", "other/e.cs")
      ),
      ("exclude a folder by its name", "sub", relative("a.cs", "b.cs", "other/e.cs")),
      ("exclude a file by its relative path", s"other${sep}e\\.cs", relative("a.cs", "b.cs", "sub/c.cs", "sub/d.cs")),
      ("exclude files with a lookahead", s"(?!sub$sep.*$$).*\\.cs", relative("sub/c.cs", "sub/d.cs")),
      (
        "not exclude anything if only a part of the path matches",
        "ub",
        relative("a.cs", "b.cs", "sub/c.cs", "sub/d.cs", "other/e.cs")
      ),
      (
        "still exclude files with a deprecated regex that only matches the absolute path",
        s"${escaped(projectUnderTest)}$sep(sub$sep.*)",
        relative("a.cs", "b.cs", "other/e.cs")
      )
    )

    forAll(testInput) { (statement, excludeRegex, expectedResult) =>
      s"$statement" in { parsedFiles(excludeRegex) shouldBe expectedResult }
    }
  }

  "Forwarding --exclude-regex to dotnetastgen" should {
    "prevent the binary from emitting a JSON for a folder matched relative to the input directory" in {
      FileUtil.usingTemporaryDirectory("csharpsrc2cpgTests") { tmpDir =>
        val config = Config()
          .withInputPath(projectUnderTest.toString)
          .withOutputPath(tmpDir.toString)
          .withIgnoredFilesRegex(s"sub$sep.*")
        new DotNetAstGenRunner(config).execute(tmpDir)
        val emitted = tmpDir.listFiles().map(_.getFileName.toString).toSet
        emitted should contain("a.json")
        emitted should not contain "sub"
      }
    }
  }

}
