package io.joern.pysrc2cpg.config

import io.joern.pysrc2cpg.Py2CpgOnFileSystem
import io.joern.pysrc2cpg.Py2CpgOnFileSystemConfig
import io.joern.x2cpg.utils.FileUtil
import io.joern.x2cpg.utils.FileUtil.*

import io.shiftleft.semanticcpg.language.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.BeforeAndAfterAll

import java.nio.file.{Paths, Files, Path}

class VenvExcludeTests extends AnyWordSpec with Matchers with TableDrivenPropertyChecks with BeforeAndAfterAll {

  private val testFiles: List[String] = List(
    ".venv/a1.py",
    ".venv/sub/s1.py",
    ".venv/pyvenv.cfg",
    ".venv2/a2.py",
    ".venv2/sub/s2.py",
    ".venv2/pyvenv.cfg",
    "folder/b.py",
    "folder/c.py",
    "main.py"
  )

  private val projectUnderTest: Path = {
    val dir = Files.createTempDirectory("pysrc2cpgTestsExcludeTest")
    testFiles.foreach { testFile =>
      val file = dir / testFile
      file.createWithParentsIfNotExists(createParents = true)
    }
    dir
  }

  override def afterAll(): Unit = FileUtil.delete(projectUnderTest, swallowIoExceptions = true)

  private def testWithArguments(ignoreVenvDir: Boolean, venvDirs: Seq[String], expectedFiles: Set[String]): Unit = {
    FileUtil.usingTemporaryDirectory("pysrc2cpgTests") { tmpDir =>
      val config = Py2CpgOnFileSystemConfig()
        .withInputPath(projectUnderTest.toString)
        .withOutputPath(tmpDir.toString)
        .withIgnoreVenvDir(ignoreVenvDir)
        .withVenvDirs(venvDirs.map(Paths.get(_)))
      val cpg = new Py2CpgOnFileSystem().createCpg(config).getOrElse(fail("No CPG was generated!"))
      cpg.file.name.l should contain theSameElementsAs expectedFiles.map(_.replace("/", java.io.File.separator))
    }
  }

  "Using different excludes via program arguments" should {

    val testInput = Table(
      // -- Header for naming all test parameters
      ("statement", "ignoreVenvDir", "venvDirs", "expectedResult"),
      // --
      // Test for --ignoreVenvDir == false:
      (
        "exclude nothing if ignoreVenvDir is false",
        false,
        Seq.empty,
        Set(
          ".venv/a1.py",
          ".venv/sub/s1.py",
          ".venv2/a2.py",
          ".venv2/sub/s2.py",
          "folder/b.py",
          "folder/c.py",
          "main.py"
        )
      ),
      // --
      // Test for --ignoreVenvDir == false with explicit venvdir:
      (
        "exclude nothing if ignoreVenvDir is false with explicit venvdir",
        false,
        Seq(".venv"),
        Set(
          ".venv/a1.py",
          ".venv/sub/s1.py",
          ".venv2/a2.py",
          ".venv2/sub/s2.py",
          "folder/b.py",
          "folder/c.py",
          "main.py"
        )
      ),
      // --
      // Test for --ignoreVenvDir == true and auto-discovery:
      (
        "exclude using auto-discovery if ignoreVenvDir is true",
        true,
        Seq.empty,
        Set("folder/b.py", "folder/c.py", "main.py")
      ),
      // --
      // Test for --ignoreVenvDir == true and no auto-discovery:
      (
        "exclude if ignoreVenvDir is true with explicit venvdir",
        true,
        Seq(".venv2"),
        Set(".venv/a1.py", ".venv/sub/s1.py", "folder/b.py", "folder/c.py", "main.py")
      ),
      // --
      // Test for --ignoreVenvDir == true and no auto-discovery with multiple venv dirs:
      (
        "exclude if ignoreVenvDir is true with explicit venvdirs",
        true,
        Seq(".venv", ".venv2"),
        Set("folder/b.py", "folder/c.py", "main.py")
      )
    )

    forAll(testInput) { (statement, ignoreVenvDir, venvDirs, result) =>
      s"$statement" in testWithArguments(ignoreVenvDir, venvDirs, result)
    }

  }

}
