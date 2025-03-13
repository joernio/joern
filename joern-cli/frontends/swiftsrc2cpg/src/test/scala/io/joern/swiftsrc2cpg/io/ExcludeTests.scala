package io.joern.swiftsrc2cpg.io

import io.joern.swiftsrc2cpg.Config
import io.joern.swiftsrc2cpg.passes.AstCreationPass
import io.joern.swiftsrc2cpg.utils.AstGenRunner
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.BeforeAndAfterAll

import java.nio.file.{Files, Path}

import java.util.regex.Pattern

class ExcludeTests extends AnyWordSpec with Matchers with TableDrivenPropertyChecks with BeforeAndAfterAll {

  private implicit val schemaValidationMode: ValidationMode = ValidationMode.Enabled

  private val testFiles: List[String] =
    List(
      "__sub/x.swift",
      "tests/x.swift",
      ".sub/e.swift",
      "folder/b.swift",
      "folder/c.swift",
      "foo.bar/d.swift",
      "a.swift",
      "index.swift"
    )

  private val projectUnderTest: Path = {
    val dir = Files.createTempDirectory("swiftsrc2cpgTestsExcludeTest")
    testFiles.foreach { testFile =>
      val file = dir / testFile
      file.createWithParentsIfNotExists(createParents = true)
    }
    dir
  }

  override def afterAll(): Unit = FileUtil.delete(projectUnderTest, swallowIoExceptions = true)

  private def testWithArguments(exclude: Seq[String], excludeRegex: String, expectedFiles: Set[String]): Unit = {
    FileUtil.usingTemporaryDirectory("swiftsrc2cpgTests") { tmpDir =>
      val cpg = newEmptyCpg()
      val config = Config()
        .withInputPath(projectUnderTest.toString)
        .withOutputPath(tmpDir.toString)
        .withIgnoredFiles(exclude)
        .withIgnoredFilesRegex(excludeRegex)
      val astGenResult = new AstGenRunner(config).execute(tmpDir)
      new AstCreationPass(cpg, astGenResult, config).createAndApply()
      cpg.file.name.l should contain theSameElementsAs expectedFiles
    }
  }

  "Using different excludes via program arguments" should {

    val testInput = Table(
      // -- Header for naming all test parameters
      ("statement", "exclude", "excludeRegex", "expectedResult"),
      // --
      // Test for default:
      (
        "exclude nothing if no excludes are given",
        Seq.empty[String],
        "",
        Set("index.swift", "a.swift", "folder/b.swift", "folder/c.swift", "foo.bar/d.swift")
      ),
      // --
      // Tests for --exclude only:
      (
        "exclude a file with --exclude with relative path",
        Seq("index.swift"),
        "",
        Set("a.swift", "folder/b.swift", "folder/c.swift", "foo.bar/d.swift")
      ),
      (
        "exclude files with --exclude with relative paths",
        Seq("index.swift", "folder/b.swift"),
        "",
        Set("a.swift", "folder/c.swift", "foo.bar/d.swift")
      ),
      (
        "exclude a file with --exclude with absolute path",
        Seq(s"$projectUnderTest/index.swift"),
        "",
        Set("a.swift", "folder/b.swift", "folder/c.swift", "foo.bar/d.swift")
      ),
      (
        "exclude files with --exclude with absolute paths",
        Seq(s"$projectUnderTest/index.swift", s"$projectUnderTest/folder/b.swift"),
        "",
        Set("a.swift", "folder/c.swift", "foo.bar/d.swift")
      ),
      (
        "exclude files with --exclude with mixed paths",
        Seq("index.swift", s"$projectUnderTest/folder/b.swift"),
        "",
        Set("a.swift", "folder/c.swift", "foo.bar/d.swift")
      ),
      (
        "exclude a folder with --exclude with absolute path",
        Seq(s"$projectUnderTest/folder/"),
        "",
        Set("a.swift", "index.swift", "foo.bar/d.swift")
      ),
      (
        "exclude a folder with --exclude with relative path",
        Seq("folder/"),
        "",
        Set("a.swift", "index.swift", "foo.bar/d.swift")
      ),
      // --
      // Tests for --exclude-regex only:
      (
        "exclude a file with --exclude-regex",
        Seq.empty,
        ".*index\\..*",
        Set("a.swift", "folder/b.swift", "folder/c.swift", "foo.bar/d.swift")
      ),
      (
        "exclude files with --exclude-regex",
        Seq.empty,
        ".*(index|b)\\..*",
        Set("a.swift", "folder/c.swift", "foo.bar/d.swift")
      ),
      (
        "exclude a complete folder with --exclude-regex",
        Seq.empty,
        s".*${Pattern.quote(java.io.File.separator)}?folder${Pattern.quote(java.io.File.separator)}.*",
        Set("index.swift", "a.swift", "foo.bar/d.swift")
      ),
      // --
      // Tests for mixed arguments
      (
        "exclude files with --exclude and --exclude-regex",
        Seq("a.swift"),
        ".*(index|b)\\..*",
        Set("folder/c.swift", "foo.bar/d.swift")
      )
    )

    forAll(testInput) { (statement, exclude, excludeRegex, result) =>
      s"$statement" in testWithArguments(exclude, excludeRegex, result)
    }

  }

}
