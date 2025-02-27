package io.joern.jssrc2cpg.io

import better.files.File as BetterFile
import io.joern.jssrc2cpg.Config
import io.joern.jssrc2cpg.passes.AstCreationPass
import io.joern.jssrc2cpg.utils.AstGenRunner
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.joern.x2cpg.utils.FileUtil
import io.joern.x2cpg.utils.FileUtil.*
import io.shiftleft.semanticcpg.language.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.BeforeAndAfterAll

import java.nio.file.{Files, Path}
import java.util.regex.Pattern

class ExcludeTests extends AnyWordSpec with Matchers with TableDrivenPropertyChecks with BeforeAndAfterAll {

  private implicit val schemaValidationMode: ValidationMode = ValidationMode.Enabled

  private val testFiles: List[String] = List(
    ".sub/e.js",
    "folder/b.js",
    "folder/c.js",
    "foo.bar/d.js",
    "tests/a.spec.js",
    "tests/b.mock.js",
    "tests/c.e2e.js",
    "tests/d.test.js",
    "a.js",
    "b-min.js",
    "c.spec.js",
    "d.chunk.js",
    "index.js"
  )

  private val projectUnderTest: Path = {
    val dir = Files.createTempDirectory("jssrc2cpgTestsExcludeTest")
    testFiles.foreach { testFile =>
      val file = dir / testFile
      file.createWithParentsIfNotExists(createParents = true)
    }
    dir
  }

  override def afterAll(): Unit = FileUtil.delete(projectUnderTest, swallowIoExceptions = true)

  private def testWithArguments(exclude: Seq[String], excludeRegex: String, expectedFiles: Set[String]): Unit = {
    BetterFile.usingTemporaryDirectory("jssrc2cpgTests") { tmpDir =>
      val cpg = newEmptyCpg()
      val config = Config(tsTypes = false)
        .withInputPath(projectUnderTest.toString)
        .withOutputPath(tmpDir.toString)
        .withIgnoredFiles(exclude)
        .withIgnoredFilesRegex(excludeRegex)
      val astGenResult = new AstGenRunner(config).execute(tmpDir)
      new AstCreationPass(cpg, astGenResult, config).createAndApply()
      cpg.file.name.l should contain theSameElementsAs expectedFiles.map(_.replace("/", java.io.File.separator))
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
        Set("index.js", "a.js", "folder/b.js", "folder/c.js", "foo.bar/d.js")
      ),
      // --
      // Tests for --exclude only:
      (
        "exclude a file with --exclude with relative path",
        Seq("index.js"),
        "",
        Set("a.js", "folder/b.js", "folder/c.js", "foo.bar/d.js")
      ),
      (
        "exclude files with --exclude with relative paths",
        Seq("index.js", "folder/b.js"),
        "",
        Set("a.js", "folder/c.js", "foo.bar/d.js")
      ),
      (
        "exclude a file with --exclude with absolute path",
        Seq(s"$projectUnderTest/index.js"),
        "",
        Set("a.js", "folder/b.js", "folder/c.js", "foo.bar/d.js")
      ),
      (
        "exclude files with --exclude with absolute paths",
        Seq(s"$projectUnderTest/index.js", s"$projectUnderTest/folder/b.js"),
        "",
        Set("a.js", "folder/c.js", "foo.bar/d.js")
      ),
      (
        "exclude files with --exclude with mixed paths",
        Seq("index.js", s"$projectUnderTest/folder/b.js"),
        "",
        Set("a.js", "folder/c.js", "foo.bar/d.js")
      ),
      (
        "exclude a folder with --exclude with absolute path",
        Seq(s"$projectUnderTest/folder/"),
        "",
        Set("a.js", "index.js", "foo.bar/d.js")
      ),
      (
        "exclude a folder with --exclude with relative path",
        Seq("folder/"),
        "",
        Set("a.js", "index.js", "foo.bar/d.js")
      ),
      // --
      // Tests for --exclude-regex only:
      (
        "exclude a file with --exclude-regex",
        Seq.empty,
        ".*index\\..*",
        Set("a.js", "folder/b.js", "folder/c.js", "foo.bar/d.js")
      ),
      ("exclude files with --exclude-regex", Seq.empty, ".*(index|b)\\..*", Set("a.js", "folder/c.js", "foo.bar/d.js")),
      (
        "exclude a complete folder with --exclude-regex",
        Seq.empty,
        s".*${Pattern.quote(java.io.File.separator)}?folder${Pattern.quote(java.io.File.separator)}.*",
        Set("index.js", "a.js", "foo.bar/d.js")
      ),
      // --
      // Tests for mixed arguments
      (
        "exclude files with --exclude and --exclude-regex",
        Seq("a.js"),
        ".*(index|b)\\..*",
        Set("folder/c.js", "foo.bar/d.js")
      )
    )

    forAll(testInput) { (statement, exclude, excludeRegex, result) =>
      s"$statement" in testWithArguments(exclude, excludeRegex, result)
    }

  }

}
