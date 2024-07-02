package io.joern.c2cpg.io

import better.files.File
import io.joern.c2cpg.Config
import io.joern.c2cpg.C2Cpg
import io.joern.x2cpg.X2Cpg
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.BeforeAndAfterAll

import java.util.regex.Pattern

class ExcludeTests extends AnyWordSpec with Matchers with TableDrivenPropertyChecks with BeforeAndAfterAll {

  private val TestFiles: List[String] =
    List(
      ".sub/e.c",
      "test/file.c",
      "tests/file.c",
      "sub/test/file.c",
      "subA/subB/test/file.c",
      "subA/subB/tests/file.c",
      "folder/b.c",
      "folder/c.c",
      "foo.bar/d.c",
      "a.c",
      "index.c",
      "sub/CMakeFiles/foo.c",
      "CMakeFiles/foo.c",
      "CMakeFiles/sub/foo.c"
    )

  private val projectUnderTest: File = {
    val dir = File.newTemporaryDirectory("c2cpgTestsExcludeTest")
    TestFiles.foreach { testFile =>
      val file = dir / testFile
      file.createIfNotExists(createParents = true)
    }
    dir
  }

  override def afterAll(): Unit = projectUnderTest.delete(swallowIOExceptions = true)

  private def testWithArguments(exclude: Seq[String], excludeRegex: String, expectedFiles: Set[String]): Unit = {
    val cpgOutFile = File.newTemporaryFile("c2cpg.bin")
    cpgOutFile.deleteOnExit()

    val config = Config()
      .withInputPath(projectUnderTest.toString)
      .withOutputPath(cpgOutFile.toString)
      .withIgnoredFiles(exclude)
      .withIgnoredFilesRegex(excludeRegex)
    val c2cpg = new C2Cpg()
    val cpg   = c2cpg.createCpg(config).get

    X2Cpg.applyDefaultOverlays(cpg)
    cpg.file.nameNot(FileTraversal.UNKNOWN, "<includes>").name.l should contain theSameElementsAs expectedFiles.map(
      _.replace("/", java.io.File.separator)
    )
  }

  "Using case sensitive excludes" should {
    "exclude the given files correctly" in {
      if (scala.util.Properties.isWin) {
        // both are written uppercase and are ignored nevertheless
        testWithArguments(Seq("Folder", "Index.c"), "", Set("a.c", "foo.bar/d.c"))
      }
      if (scala.util.Properties.isMac) {
        // Folder written uppercase and it is not ignored while Index.c is.
        // This might be an issue within Files.isSameFile but we take it for now.
        testWithArguments(Seq("Folder", "Index.c"), "", Set("a.c", "folder/b.c", "folder/c.c", "foo.bar/d.c"))
      }
      if (scala.util.Properties.isLinux) {
        // both are written uppercase and are not ignored
        testWithArguments(
          Seq("Folder", "Index.c"),
          "",
          Set("a.c", "folder/b.c", "folder/c.c", "foo.bar/d.c", "index.c")
        )
      }
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
        Set("index.c", "a.c", "folder/b.c", "folder/c.c", "foo.bar/d.c")
      ),
      // --
      // Tests for --exclude only:
      (
        "exclude a file with --exclude with relative path",
        Seq("index.c"),
        "",
        Set("a.c", "folder/b.c", "folder/c.c", "foo.bar/d.c")
      ),
      (
        "exclude files with --exclude with relative paths",
        Seq("index.c", "folder/b.c"),
        "",
        Set("a.c", "folder/c.c", "foo.bar/d.c")
      ),
      (
        "exclude a file with --exclude with absolute path",
        Seq(s"$projectUnderTest/index.c"),
        "",
        Set("a.c", "folder/b.c", "folder/c.c", "foo.bar/d.c")
      ),
      (
        "exclude files with --exclude with absolute paths",
        Seq(s"$projectUnderTest/index.c", s"$projectUnderTest/folder/b.c"),
        "",
        Set("a.c", "folder/c.c", "foo.bar/d.c")
      ),
      (
        "exclude files with --exclude with mixed paths",
        Seq("index.c", s"$projectUnderTest/folder/b.c"),
        "",
        Set("a.c", "folder/c.c", "foo.bar/d.c")
      ),
      (
        "exclude a folder with --exclude with absolute path",
        Seq(s"$projectUnderTest/folder/"),
        "",
        Set("a.c", "index.c", "foo.bar/d.c")
      ),
      ("exclude a folder with --exclude with relative path", Seq("folder/"), "", Set("a.c", "index.c", "foo.bar/d.c")),
      // --
      // Tests for --exclude-regex only:
      (
        "exclude a file with --exclude-regex",
        Seq.empty,
        ".*index\\..*",
        Set("a.c", "folder/b.c", "folder/c.c", "foo.bar/d.c")
      ),
      ("exclude files with --exclude-regex", Seq.empty, ".*(index|b)\\..*", Set("a.c", "folder/c.c", "foo.bar/d.c")),
      (
        "exclude a complete folder with --exclude-regex",
        Seq.empty,
        s".*${Pattern.quote(java.io.File.separator)}?folder${Pattern.quote(java.io.File.separator)}.*",
        Set("index.c", "a.c", "foo.bar/d.c")
      ),
      // --
      // Tests for mixed arguments
      (
        "exclude files with --exclude and --exclude-regex",
        Seq("a.c"),
        ".*(index|b)\\..*",
        Set("folder/c.c", "foo.bar/d.c")
      )
    )

    forAll(testInput) { (statement, exclude, excludeRegex, result) =>
      s"$statement" in testWithArguments(exclude, excludeRegex, result)
    }

  }

}
