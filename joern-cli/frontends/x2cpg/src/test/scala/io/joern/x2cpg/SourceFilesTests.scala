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

import java.nio.file.{Files, Paths}

class SourceFilesTests extends AnyWordSpec with Matchers with Inside {

  private val cSourceFileExtensions: Set[String] = Set(".c", ".h")
  private val resourcesRoot: String = ProjectRoot.relativise("joern-cli/frontends/x2cpg/src/main/resources")

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
        val symlink = Files.createSymbolicLink(tmpDir / "broken.c", Paths.get("does/not/exist.c"))
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

  "filterFile" should {

    "accept a file whose size is lower than maxFileSize" in {
      FileUtil.usingTemporaryDirectory() { tmpDir =>
        val file  = tmpDir / "a.c"
        val bytes = Array.fill[Byte](1023)(0)
        Files.write(file, bytes)

        SourceFiles.filterFile(file = file.toString, inputPath = tmpDir.toString, maxFileSize = 1024L) shouldBe true
      }
    }

    "accept a file whose size equals maxFileSize" in {
      FileUtil.usingTemporaryDirectory() { tmpDir =>
        val file  = tmpDir / "a.c"
        val bytes = Array.fill[Byte](1024)(0)
        Files.write(file, bytes)

        SourceFiles.filterFile(file = file.toString, inputPath = tmpDir.toString, maxFileSize = 1024L) shouldBe true
      }
    }

    "reject a file whose size exceeds maxFileSize" in {
      FileUtil.usingTemporaryDirectory() { tmpDir =>
        val file  = tmpDir / "a.c"
        val bytes = Array.fill[Byte](1025)(0)
        Files.write(file, bytes)

        SourceFiles.filterFile(file = file.toString, inputPath = tmpDir.toString, maxFileSize = 1024L) shouldBe false
      }
    }

  }

  "parseMaxFileSize" should {

    "parse plain bytes" in {
      SourceFiles.parseMaxFileSize("123") shouldBe Some(123L)
    }

    "parse MB/GB suffixes case-insensitively (and tolerate whitespace)" in {
      SourceFiles.parseMaxFileSize("1MB") shouldBe Some(1024L * 1024L)
      SourceFiles.parseMaxFileSize("2gb") shouldBe Some(2L * 1024L * 1024L * 1024L)
      SourceFiles.parseMaxFileSize("  3  MB ") shouldBe Some(3L * 1024L * 1024L)
    }

    "return None for invalid values" in {
      SourceFiles.parseMaxFileSize("") shouldBe None
      SourceFiles.parseMaxFileSize(" ") shouldBe None
      SourceFiles.parseMaxFileSize("-1GB") shouldBe None
      SourceFiles.parseMaxFileSize("1TB") shouldBe None
      SourceFiles.parseMaxFileSize("GB") shouldBe None
    }

    "parse decimal MB/GB values" in {
      SourceFiles.parseMaxFileSize("1.5GB") shouldBe Some((1.5 * 1024 * 1024 * 1024).round)
      SourceFiles.parseMaxFileSize("0.25MB") shouldBe Some((0.25 * 1024 * 1024).round)
      SourceFiles.parseMaxFileSize(" 0.5 gb ") shouldBe Some((0.5 * 1024 * 1024 * 1024).round)
    }
  }

  "formatMaxFileSize" should {

    "format GiB and MiB nicely" in {
      SourceFiles.formatMaxFileSize(2L * 1024L * 1024L * 1024L) shouldBe "2GB"
      SourceFiles.formatMaxFileSize(512L * 1024L * 1024L) shouldBe "512MB"
    }

    "support rounded decimals" in {
      // 1.5 GiB
      SourceFiles.formatMaxFileSize((1.5 * 1024 * 1024 * 1024).toLong) shouldBe "1.5GB"
      // 1.2 MiB (rounded to 1 decimal)
      SourceFiles.formatMaxFileSize((1.24 * 1024 * 1024).toLong) shouldBe "1.2MB"
    }

    "fall back to bytes if below 1MiB" in {
      SourceFiles.formatMaxFileSize(12345L) shouldBe "12345B"
    }
  }

}
