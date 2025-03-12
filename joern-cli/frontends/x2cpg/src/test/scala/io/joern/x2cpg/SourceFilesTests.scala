package io.joern.x2cpg

import io.joern.x2cpg.utils.IgnoreInWindows
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*

import io.shiftleft.utils.ProjectRoot
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.Inside

import java.nio.file.attribute.PosixFilePermissions
import scala.util.Try
import java.io.FileNotFoundException

import java.nio.file.{Files, Path, Paths}

class SourceFilesTests extends AnyWordSpec with Matchers with Inside {

  val cSourceFileExtensions = Set(".c", ".h")
  val resourcesRoot         = ProjectRoot.relativise("joern-cli/frontends/x2cpg/src/main/resources")

  "determine source files" when {

    "using regular input file" in {
      SourceFiles.determine(s"$resourcesRoot/testcode/main.c", cSourceFileExtensions).size shouldBe 1
    }

    "using regular input directory" in {
      SourceFiles.determine(s"$resourcesRoot/testcode", cSourceFileExtensions).size shouldBe 3
    }

    "input is symlink to file" in {
      SourceFiles.determine(s"$resourcesRoot/symlink-to-main.c", cSourceFileExtensions).size shouldBe 1
    }

    "input is symlink to directory" in {
      SourceFiles.determine(s"$resourcesRoot/symlink-to-testcode", cSourceFileExtensions).size shouldBe 3
    }

    "ignoreByRegex is used" in {
      SourceFiles
        .determine(s"$resourcesRoot/testcode", cSourceFileExtensions, ignoredFilesRegex = Some(".*[.]h".r))
        .size shouldBe 1
    }

    "ignoreByFilePath is used" in {
      SourceFiles
        .determine(
          s"$resourcesRoot/testcode",
          cSourceFileExtensions,
          ignoredFilesPath = Some(Seq(Paths.get(s"$resourcesRoot/testcode").toString))
        )
        .size shouldBe 0
    }

  }

  "do not throw an exception" when {
    "one of the input files is a broken symlink" in {
      FileUtil.usingTemporaryDirectory() { tmpDir =>
        (tmpDir / "a.c").createWithParentsIfNotExists()
        val symlink = Files.createSymbolicLink((tmpDir / "broken.c"), Paths.get("does/not/exist.c"))
        Files.exists(symlink) shouldBe false
        Files.isReadable(symlink) shouldBe false

        val ignored = (tmpDir / "ignored.c").createWithParentsIfNotExists()
        val result = Try(
          SourceFiles
            .determine(
              tmpDir.absolutePathAsString,
              cSourceFileExtensions,
              ignoredFilesPath = Some(Seq(ignored.toString))
            )
        )
        result.isFailure shouldBe false
        result.getOrElse(List.empty).size shouldBe 1
      }
    }
  }

  "throw an exception" when {

    "the input file does not exist" in {
      val result = Try(SourceFiles.determine("path/to/nothing/", cSourceFileExtensions))
      result.isFailure shouldBe true
      result.failed.get shouldBe a[FileNotFoundException]
    }

    "the input file exists, but is not readable" taggedAs IgnoreInWindows in {
      FileUtil.usingTemporaryFile() { tmpFile =>
        Files.setPosixFilePermissions(tmpFile, PosixFilePermissions.fromString("-wx-w--w-"))
        Files.exists(tmpFile) shouldBe true
        Files.isReadable(tmpFile) shouldBe false

        val result = Try(SourceFiles.determine(tmpFile.absolutePathAsString, cSourceFileExtensions))
        result.isFailure shouldBe true
        result.failed.get shouldBe a[FileNotFoundException]
      }
    }
  }
}
