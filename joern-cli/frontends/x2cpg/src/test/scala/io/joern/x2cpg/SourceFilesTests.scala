package io.joern.x2cpg

import better.files.*
import io.joern.x2cpg.utils.IgnoreInWindows
import io.shiftleft.utils.ProjectRoot
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.Inside

import java.nio.file.attribute.PosixFilePermissions
import scala.jdk.CollectionConverters.*
import scala.util.Try
import java.io.FileNotFoundException

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
          ignoredFilesPath = Some(Seq(File(s"$resourcesRoot/testcode").pathAsString))
        )
        .size shouldBe 0
    }

  }

  "do not throw an exception" when {
    "one of the input files is a broken symlink" in {
      File.usingTemporaryDirectory() { tmpDir =>
        (tmpDir / "a.c").touch()
        val symlink = (tmpDir / "broken.c").symbolicLinkTo(File("does/not/exist.c"))
        symlink.exists shouldBe false
        symlink.isReadable shouldBe false
        val ignored = (tmpDir / "ignored.c").touch()
        val result = Try(
          SourceFiles
            .determine(tmpDir.canonicalPath, cSourceFileExtensions, ignoredFilesPath = Some(Seq(ignored.pathAsString)))
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
      File.usingTemporaryFile() { tmpFile =>
        tmpFile.setPermissions(PosixFilePermissions.fromString("-wx-w--w-").asScala.toSet)
        tmpFile.exists shouldBe true
        tmpFile.isReadable shouldBe false

        val result = Try(SourceFiles.determine(tmpFile.canonicalPath, cSourceFileExtensions))
        result.isFailure shouldBe true
        result.failed.get shouldBe a[FileNotFoundException]
      }
    }
  }
}
