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

  private object WindowsOnly
      extends Tag(
        if (scala.util.Properties.isWin) ""
        else classOf[Ignore].getName
      )

  private object UnixOnly
      extends Tag(
        if (!scala.util.Properties.isWin) ""
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

  "toRelativePath" should {

    "handle basic Unix filesystem paths" when {

      "path is directly inside rootPath" taggedAs UnixOnly in {
        val rootPath = "/home/user/project"
        val filePath = "/home/user/project/src/main.c"
        SourceFiles.toRelativePath(filePath, rootPath) shouldBe "src/main.c"
      }

      "path is deeply nested inside rootPath" taggedAs UnixOnly in {
        val rootPath = "/home/user/project"
        val filePath = "/home/user/project/src/main/scala/io/joern/Main.scala"
        SourceFiles.toRelativePath(filePath, rootPath) shouldBe "src/main/scala/io/joern/Main.scala"
      }

      "path is the same as rootPath" taggedAs UnixOnly in {
        val rootPath = "/home/user/project"
        val filePath = "/home/user/project"
        // When paths are equal, should return just the file name component
        val result = SourceFiles.toRelativePath(filePath, rootPath)
        result should not be empty
      }

      "path is outside rootPath" taggedAs UnixOnly in {
        val rootPath = "/home/user/project"
        val filePath = "/home/other/file.c"
        // Should return the path unaltered when not inside rootPath
        SourceFiles.toRelativePath(filePath, rootPath) shouldBe filePath
      }

      "path is a sibling directory to rootPath" taggedAs UnixOnly in {
        val rootPath = "/home/user/project"
        val filePath = "/home/user/other/file.c"
        SourceFiles.toRelativePath(filePath, rootPath) shouldBe filePath
      }

      "path is a parent directory of rootPath" taggedAs UnixOnly in {
        val rootPath = "/home/user/project"
        val filePath = "/home/user"
        SourceFiles.toRelativePath(filePath, rootPath) shouldBe filePath
      }

      "path contains dot segments that normalize correctly" taggedAs UnixOnly in {
        val rootPath = "/home/user/project"
        val filePath = "/home/user/project/src/../src/./main.c"
        // After normalization, should become src/main.c
        SourceFiles.toRelativePath(filePath, rootPath) shouldBe "src/main.c"
      }

      "rootPath has trailing slash" taggedAs UnixOnly in {
        val rootPath = "/home/user/project/"
        val filePath = "/home/user/project/src/main.c"
        SourceFiles.toRelativePath(filePath, rootPath) shouldBe "src/main.c"
      }
    }

    "handle Windows 8.3 short file names" when {

      "path contains 8.3 format directory names" taggedAs WindowsOnly in {
        val rootPath = "C:\\PROGRA~1\\MyApp"
        val filePath = "C:\\PROGRA~1\\MyApp\\src\\main.c"
        // When both use short names consistently, should relativize (normalized to forward slashes)
        SourceFiles.toRelativePath(filePath, rootPath) shouldBe "src\\main.c"
      }

      "mixing short and long file names" taggedAs WindowsOnly in {
        // Use real Windows paths - Program Files exists on all Windows systems
        // PROGRA~1 is the 8.3 short name for "Program Files"
        // Common Files subdirectory also typically exists
        val rootPath = "C:\\Program Files"
        val filePath = "C:\\PROGRA~1\\Common Files\\"
        val result   = SourceFiles.toRelativePath(filePath, rootPath)
        // toRealPath() will resolve both to the same canonical path
        result shouldBe "Common Files"
      }

      "short name in file component" taggedAs WindowsOnly in {
        val rootPath = "C:\\Users\\LONGUS~1\\project"
        val filePath = "C:\\Users\\LONGUS~1\\project\\SOMEDOC~1.TXT"
        // Consistent short names should work
        SourceFiles.toRelativePath(filePath, rootPath) shouldBe "SOMEDOC~1.TXT"
      }

      "multiple levels of short names" taggedAs WindowsOnly in {
        val rootPath = "C:\\PROGRA~1\\COMMON~1\\MyApp"
        val filePath = "C:\\PROGRA~1\\COMMON~1\\MyApp\\CONFIG~1\\SETTIN~1.INI"
        // Consistent short names throughout should relativize
        SourceFiles.toRelativePath(filePath, rootPath) shouldBe "CONFIG~1\\SETTIN~1.INI"
      }

    }

    "handle case-insensitive paths" when {

      "path differs only in case on Windows-style paths" taggedAs WindowsOnly in {
        val rootPath = "C:\\Users\\Project"
        val filePath = "C:\\users\\project\\src\\main.c"
        // On case-insensitive filesystems like Windows, these refer to same location
        // Should recognize equivalence and relativize (normalized to forward slashes)
        SourceFiles.toRelativePath(filePath, rootPath) shouldBe "src\\main.c"
      }

      "path differs in case mid-path" taggedAs WindowsOnly in {
        val rootPath = "C:\\Users\\MyUser\\Project"
        val filePath = "C:\\Users\\myuser\\project\\src\\Main.C"
        // Should handle case differences throughout the path
        SourceFiles.toRelativePath(filePath, rootPath) shouldBe "src\\Main.C"
      }

      "case difference in root directory" taggedAs WindowsOnly in {
        val rootPath = "C:\\PROJECT"
        val filePath = "C:\\project\\src\\file.c"
        // Drive letters and paths are case-insensitive on Windows
        SourceFiles.toRelativePath(filePath, rootPath) shouldBe "src\\file.c"
      }

      "Unix-style paths with case differences (should be treated as different)" taggedAs UnixOnly in {
        val rootPath = "/home/user/Project"
        val filePath = "/home/user/project/src/main.c"
        // On case-sensitive filesystems like Linux, these are different paths
        SourceFiles.toRelativePath(filePath, rootPath) shouldBe "/home/user/project/src/main.c"
      }

      "mixed case in file extension" taggedAs WindowsOnly in {
        val rootPath = "C:\\Users\\Project"
        val filePath = "C:\\Users\\Project\\file.C"
        // Extension case shouldn't matter on Windows
        SourceFiles.toRelativePath(filePath, rootPath) shouldBe "file.C"
      }
    }

    "handle edge cases" when {

      "rootPath is root directory /" taggedAs UnixOnly in {
        val rootPath = "/"
        val filePath = "/home/user/project/file.c"
        // Everything is under /, so should relativize
        SourceFiles.toRelativePath(filePath, rootPath) shouldBe "home/user/project/file.c"
      }

      "rootPath is Windows root C:\\" taggedAs WindowsOnly in {
        val rootPath = "C:\\"
        val filePath = "C:\\Users\\project\\file.c"
        // Everything on C: drive is under C:\
        SourceFiles.toRelativePath(filePath, rootPath) shouldBe "Users\\project\\file.c"
      }

      "paths with special characters" taggedAs UnixOnly in {
        val rootPath = "/home/user/my project (2024)"
        val filePath = "/home/user/my project (2024)/src/main.c"
        // Should handle spaces and parentheses
        SourceFiles.toRelativePath(filePath, rootPath) shouldBe "src/main.c"
      }

      "path with trailing separators in both" taggedAs UnixOnly in {
        val rootPath = "/home/user/project/"
        val filePath = "/home/user/project/src/"
        // Should handle trailing slashes correctly
        SourceFiles.toRelativePath(filePath, rootPath) shouldBe "src"
      }

      "different drive letters on Windows" taggedAs WindowsOnly in {
        val rootPath = "C:\\project"
        val filePath = "D:\\other\\file.c"
        // Different drives - cannot relativize
        SourceFiles.toRelativePath(filePath, rootPath) shouldBe "D:\\other\\file.c"
      }

      "UNC paths on Windows" taggedAs WindowsOnly in {
        val rootPath = "\\\\server\\share\\project"
        val filePath = "\\\\server\\share\\project\\src\\main.c"
        // UNC paths should work
        SourceFiles.toRelativePath(filePath, rootPath) shouldBe "src\\main.c"
      }

    }
  }

}
