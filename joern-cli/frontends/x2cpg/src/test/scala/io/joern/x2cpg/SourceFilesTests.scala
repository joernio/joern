package io.joern.x2cpg

import io.joern.x2cpg.utils.IgnoreInWindows
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.utils.ProjectRoot
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.{Ignore, Inside, Tag}

import java.nio.file.attribute.PosixFilePermissions
import scala.util.Try
import java.io.FileNotFoundException
import java.nio.file.{Files, Paths}

class SourceFilesTests extends AnyWordSpec with Matchers with Inside {

  private object NotInMacOS
      extends Tag(
        if (!scala.util.Properties.isMac) ""
        else classOf[Ignore].getName
      )

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

    "accept a file whose size is lower than DefaultMaxFileSizeBytes" in {
      FileUtil.usingTemporaryDirectory() { tmpDir =>
        val file  = tmpDir / "a.c"
        val bytes = Array.fill[Byte](1024)(0)
        Files.write(file, bytes)

        SourceFiles.filterFile(file = file.toString, inputPath = tmpDir.toString) shouldBe true
      }
    }

    // This test causes OutOfMemoryError on MacOS, see: https://github.com/actions/runner-images/issues/11899
    // The test is still relevant to ensure that we do not attempt to process files larger than JVM String max. size,
    // but we need to find a way to run it without causing OOM on MacOS CI runners.
    // TODO: enable, once the issue with OOM on MacOS CI runners is resolved
    "allow a file whose size is exactly JVM String max. size" taggedAs NotInMacOS in {
      FileUtil.usingTemporaryDirectory() { tmpDir =>
        val file = tmpDir / "a.c"
        // max size for Array[Byte] in JVM is Integer.MAX_VALUE, but String can hold at most Integer.MAX_VALUE - 2 bytes
        val bytes = Array.fill[Byte](Integer.MAX_VALUE - 2)(0)
        Files.write(file, bytes)

        SourceFiles.filterFile(file = file.toString, inputPath = tmpDir.toString) shouldBe true
      }
    }

    // This test causes OutOfMemoryError on MacOS, see: https://github.com/actions/runner-images/issues/11899
    // The test is still relevant to ensure that we do not attempt to process files larger than JVM String max. size,
    // but we need to find a way to run it without causing OOM on MacOS CI runners.
    // TODO: enable, once the issue with OOM on MacOS CI runners is resolved
    "reject a file whose size is larger than JVM String max. size" taggedAs NotInMacOS in {
      FileUtil.usingTemporaryDirectory() { tmpDir =>
        val file           = tmpDir / "a.c"
        val bytes          = Array.fill[Byte](Integer.MAX_VALUE - 2)(0)
        val additionalByte = Array.fill[Byte](1)(0)
        Files.write(file, bytes)
        Files.write(file, additionalByte, java.nio.file.StandardOpenOption.APPEND)

        SourceFiles.filterFile(file = file.toString, inputPath = tmpDir.toString) shouldBe false
      }
    }

  }

}
