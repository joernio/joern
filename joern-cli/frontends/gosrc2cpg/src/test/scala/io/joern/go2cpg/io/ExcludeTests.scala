package io.joern.go2cpg.io

import io.joern.gosrc2cpg.{Config, GoSrc2Cpg}
import io.joern.gosrc2cpg.utils.GoAstGenRunner
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.wordspec.AnyWordSpec

import java.io.File
import java.nio.file.{Files, Path}
import java.util.regex.Pattern

class ExcludeTests extends AnyWordSpec with Matchers with TableDrivenPropertyChecks with BeforeAndAfterAll {

  private val testFiles: Map[String, String] = Map(
    "go.mod"      -> "module example.com/m\n\ngo 1.21\n",
    "main.go"     -> "package main\n\nfunc main() {}\n",
    "pkg/a/a.go"  -> "package a\n\nfunc Foo() int { return 1 }\n",
    "pkg/b/bb.go" -> "package b\n\nfunc Bar() int { return 2 }\n"
  )

  private val projectUnderTest: Path = {
    val dir = Files.createTempDirectory("gosrc2cpgTestsExcludeTest")
    testFiles.toSeq.sortBy(_._1).foreach { case (testFile, content) =>
      val file = dir / testFile
      file.createWithParentsIfNotExists(createParents = true)
      Files.writeString(file, content)
    }
    dir
  }

  override def afterAll(): Unit = FileUtil.delete(projectUnderTest, swallowIoExceptions = true)

  private val sep = Pattern.quote(File.separator)

  private def testWithExcludeRegex(excludeRegex: String, expectedFiles: Set[String]): Unit = {
    FileUtil.usingTemporaryDirectory("gosrc2cpgTests") { tmpDir =>
      val config = Config()
        .withInputPath(projectUnderTest.toString)
        .withOutputPath((tmpDir / "cpg.bin").toString)
        .withIgnoredFilesRegex(excludeRegex)
      val cpg   = new GoSrc2Cpg().createCpg(config).get
      val files = cpg.file.name.filter(_.endsWith(".go")).l
      cpg.close()
      files should contain theSameElementsAs expectedFiles.map(_.replace("/", File.separator))
    }
  }

  "Using --exclude-regex" should {

    val allFiles = Set("main.go", "pkg/a/a.go", "pkg/b/bb.go")

    val testInput = Table(
      ("statement", "excludeRegex", "expectedResult"),
      ("exclude nothing if no regex is given", "", allFiles),
      ("exclude a file whose relative path fully matches", "main\\.go", Set("pkg/a/a.go", "pkg/b/bb.go")),
      ("exclude a folder whose relative path fully matches", s"pkg${sep}a$sep.*", Set("main.go", "pkg/b/bb.go")),
      ("not exclude a file whose relative path only partially matches", "ain", allFiles),
      ("not exclude a file whose folder only partially matches", s"kg${sep}a", allFiles),
      ("not exclude a file whose file name only partially matches", "bb", allFiles),
      (
        "not exclude a file whose absolute path matches",
        Pattern.quote((projectUnderTest / "main.go").toString),
        allFiles
      ),
      ("not exclude a file whose generated AST file name matches", ".*\\.go\\.json", allFiles),
      ("support Java regex features such as lookahead", s"(?!pkg${sep}a$sep).*\\.go", Set("pkg/a/a.go"))
    )

    forAll(testInput) { (statement, excludeRegex, expectedResult) =>
      statement in testWithExcludeRegex(excludeRegex, expectedResult)
    }
  }

  "Forwarding --exclude-regex to goastgen" should {
    "prevent goastgen from emitting an AST file for excluded files only" in {
      FileUtil.usingTemporaryDirectory("gosrc2cpgTests") { tmpDir =>
        val config = Config()
          .withInputPath(projectUnderTest.toString)
          .withIgnoredFilesRegex(s"main\\.go|pkg${sep}a$sep.*|bb")
        new GoAstGenRunner(config).executeForGo(tmpDir)
        val emittedFiles = tmpDir.walk().filter(Files.isRegularFile(_)).map(tmpDir.relativize(_).toString).toSet
        emittedFiles shouldBe Set("go.mod.json", "pkg/b/bb.go.json").map(_.replace("/", File.separator))
      }
    }
  }

}
