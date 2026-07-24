package io.joern.javasrc2cpg.util

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.io.File
import java.nio.file.{Path, Paths}

class DelombokStderrFilterTests extends AnyWordSpec with Matchers {

  private val inputPath: Path     = Paths.get("/tmp/delombok-input").toAbsolutePath.normalize()
  private val relativeRootA: Path = Path.of("src/main/root-a")
  private val relativeRootB: Path = Path.of("src/main/root-b")
  private val absoluteRootA: Path = inputPath.resolve(relativeRootA).toAbsolutePath.normalize()
  private val absoluteRootB: Path = inputPath.resolve(relativeRootB).toAbsolutePath.normalize()

  private val fileInRootA: Path =
    absoluteRootA.resolve("com/example/FileA.java")

  private val fileInRootB: Path =
    absoluteRootB.resolve("com/example/FileB.java")

  "DelombokStderrFilter" should {

    "drop all peer-root errors and preserve JVM warnings when current root is root-b" in {
      val stderr = Seq(
        "WARNING: A restricted method in java.lang.System has been called",
        "WARNING: java.lang.System::load has been called by com.zaxxer.nuprocess.internal.LibJava10",
        s"$fileInRootB:19: error: cannot find symbol",
        "        log.info(\"hello\");",
        "        ^",
        "  symbol:   variable log",
        "  location: class com.example.UserRecord",
        s"$fileInRootB:25: error: cannot find symbol",
        "        return log;",
        "               ^",
        "  symbol:   variable log",
        "  location: class com.example.UserRecord",
        s"$fileInRootB:19: error: cannot find symbol",
        "        UserRecord u = UserRecord.builder().build();",
        "                                 ^",
        "  symbol:   method builder()",
        "  location: class com.example.UserRecord",
        s"$fileInRootB:20: error: cannot find symbol",
        "        String name = subject.getName();",
        "                             ^",
        "  symbol:   method getName()",
        "  location: variable subject of type com.example.UserRecord",
        s"$fileInRootB:30: error: cannot find symbol",
        "        this.log.warn(\"boom\");",
        "             ^",
        "  symbol:   variable log",
        "  location: class com.example.UserRecord",
        s"$fileInRootA:35: error: cannot find symbol",
        "        log.error(\"oops\");",
        "        ^",
        "  symbol:   variable log",
        "  location: class com.example.UserRecord",
        s"$fileInRootB:42: error: cannot find symbol",
        "        this.log.warn(\"boom\");",
        "             ^",
        "  symbol:   variable log",
        "  location: class com.example.UserRecord",
        s"File: $fileInRootA [delomboked]",
        s"File: $fileInRootB [unchanged]"
      )

      val filtered = DelombokStderrFilter.filter(absoluteRootA, stderr)

      filtered shouldBe Seq(
        "WARNING: A restricted method in java.lang.System has been called",
        "WARNING: java.lang.System::load has been called by com.zaxxer.nuprocess.internal.LibJava10",
        s"$fileInRootA:35: error: cannot find symbol",
        "        log.error(\"oops\");",
        "        ^",
        "  symbol:   variable log",
        "  location: class com.example.UserRecord",
        s"File: $fileInRootA [delomboked]",
        s"File: $fileInRootB [unchanged]"
      )
    }

    "keep non-peer errors" in {
      // Same UserRecord errors, but now we're delomboking root-a itself — so UserRecord is NOT in a peer root
      // and the errors must be kept. (This is a hypothetical, since delombok wouldn't emit these when
      // UserRecord is the input, but we're testing the routing.)
      val stderr = Seq(
        s"$fileInRootB:19: error: cannot find symbol",
        "        log.info(\"hello\");",
        "        ^",
        "  symbol:   variable log",
        "  location: class com.example.UserRecord"
      )

      val filtered = DelombokStderrFilter.filter(absoluteRootB, stderr)

      filtered shouldBe stderr
    }

    "parse Windows-style diagnostic headers and drop peer-root records" in {
      // Path.of/Paths.get with a Windows drive-letter string only produces a proper absolute path on
      // Windows; on POSIX it becomes a relative fragment. Skip on non-Windows platforms.
      assume(File.separatorChar == '\\')
      // Synthetic setup with drive-letter absolute roots. The filter should parse the header and route the
      // location FQN through the index to conclude the record is a peer-root false positive.
      val winInput: Path         = Paths.get("C:\\repo\\proj").toAbsolutePath.normalize()
      val winRelativeRootA: Path = Path.of("src/main/java")
      val winRelativeRootB: Path = Path.of("src/main/java-report")
      val winAbsoluteRootA: Path = winInput.resolve(winRelativeRootA).toAbsolutePath.normalize()
      val winAbsoluteRootB: Path = winInput.resolve(winRelativeRootB).toAbsolutePath.normalize()
      val winFileInA: Path       = winAbsoluteRootA.resolve("com/example/a/FileA.java")
      val winFileInB: Path       = winAbsoluteRootB.resolve("com/example/b/FileB.java")

      val stderr = Seq(
        s"$winFileInA:19: error: cannot find symbol",
        "        UserRecord u = UserRecord.builder().build();",
        "                                 ^",
        "  symbol:   method builder()",
        "  location: class com.example.UserRecord"
      )

      val filtered = DelombokStderrFilter.filter(winAbsoluteRootB, stderr)

      filtered shouldBe empty
    }
  }
}
