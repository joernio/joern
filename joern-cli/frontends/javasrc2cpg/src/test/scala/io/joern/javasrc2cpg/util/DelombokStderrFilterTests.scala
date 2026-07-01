package io.joern.javasrc2cpg.util

import io.joern.javasrc2cpg.util.DelombokStderrFilter.FqnIndex
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.io.File
import java.nio.file.{Path, Paths}

class DelombokStderrFilterTests extends AnyWordSpec with Matchers {

  // Two synthetic absolute package roots that share an input path. The current root is `root-b`, the peer is
  // `root-a`. `com.example.UserRecord` lives in `root-a` (as its source stem); `com.example.UserService` and
  // `com.example.report.UserReport` live in `root-b`.
  private val inputPath: Path     = Paths.get("/tmp/delombok-input").toAbsolutePath.normalize()
  private val relativeRootA: Path = Path.of("src/main/java")
  private val relativeRootB: Path = Path.of("src/main/java-report")
  private val absoluteRootA: Path = inputPath.resolve(relativeRootA).toAbsolutePath.normalize()
  private val absoluteRootB: Path = inputPath.resolve(relativeRootB).toAbsolutePath.normalize()

  private val userRecordRelativePath: Path =
    relativeRootA.resolve("com/example/UserRecord.java")
  private val userServiceRelativePath: Path =
    relativeRootA.resolve("com/example/UserService.java")
  private val userReportRelativePath: Path =
    relativeRootB.resolve("com/example/report/UserReport.java")

  private val absUserRecord = absoluteRootA.resolve("com/example/UserRecord.java")
  private val absUserReport = absoluteRootB.resolve("com/example/report/UserReport.java")

  private val fileInfo: List[SourceParser.FileInfo] = List(
    SourceParser.FileInfo(userRecordRelativePath, Some("com.example"), usesLombok = true),
    SourceParser.FileInfo(userServiceRelativePath, Some("com.example"), usesLombok = true),
    SourceParser.FileInfo(userReportRelativePath, Some("com.example.report"), usesLombok = true)
  )

  private val packageRoots = List(relativeRootA, relativeRootB)
  private val index        = FqnIndex.build(inputPath, fileInfo, packageRoots)

  "DelombokStderrFilter" should {

    "drop all peer-root errors and preserve JVM warnings when current root is root-b" in {
      // Six diagnostics from the reproducer: four in UserRecord.java (root-a), two in UserReport.java (root-b).
      // When we're delomboking root-b, the peer is root-a — so all UserRecord errors (whose files live under
      // root-a) are false positives and must be dropped. The UserReport errors reference
      // `com.example.UserRecord.builder` / `com.example.UserRecord` (a peer-root FQN) and must also be dropped.
      val stderr = Seq(
        "WARNING: A restricted method in java.lang.System has been called",
        "WARNING: java.lang.System::load has been called by com.zaxxer.nuprocess.internal.LibJava10",
        s"$absUserRecord:19: error: cannot find symbol",
        "        log.info(\"hello\");",
        "        ^",
        "  symbol:   variable log",
        "  location: class com.example.UserRecord",
        s"$absUserRecord:25: error: cannot find symbol",
        "        return log;",
        "               ^",
        "  symbol:   variable log",
        "  location: class com.example.UserRecord",
        s"$absUserReport:19: error: cannot find symbol",
        "        UserRecord u = UserRecord.builder().build();",
        "                                 ^",
        "  symbol:   method builder()",
        "  location: class com.example.UserRecord",
        s"$absUserReport:20: error: cannot find symbol",
        "        String name = subject.getName();",
        "                             ^",
        "  symbol:   method getName()",
        "  location: variable subject of type com.example.UserRecord",
        s"$absUserRecord:30: error: cannot find symbol",
        "        this.log.warn(\"boom\");",
        "             ^",
        "  symbol:   variable log",
        "  location: class com.example.UserRecord",
        s"$absUserRecord:35: error: cannot find symbol",
        "        log.error(\"oops\");",
        "        ^",
        "  symbol:   variable log",
        "  location: class com.example.UserRecord"
      )

      val filtered = DelombokStderrFilter.filter(absoluteRootB, Seq(absoluteRootA), index, stderr)

      filtered shouldBe Seq(
        "WARNING: A restricted method in java.lang.System has been called",
        "WARNING: java.lang.System::load has been called by com.zaxxer.nuprocess.internal.LibJava10"
      )
    }

    "keep non-peer errors" in {
      // Same UserRecord errors, but now we're delomboking root-a itself — so UserRecord is NOT in a peer root
      // and the errors must be kept. (This is a hypothetical, since delombok wouldn't emit these when
      // UserRecord is the input, but we're testing the routing.)
      val stderr = Seq(
        s"$absUserRecord:19: error: cannot find symbol",
        "        log.info(\"hello\");",
        "        ^",
        "  symbol:   variable log",
        "  location: class com.example.UserRecord"
      )

      val filtered = DelombokStderrFilter.filter(absoluteRootA, Seq(absoluteRootB), index, stderr)

      filtered shouldBe stderr
    }

    "pass through unclassified diagnostics" in {
      val stderr = Seq(
        s"$absUserReport:5: error: something new and unrecognised",
        "  more context we don't parse",
        "an entirely freeform line"
      )

      val filtered = DelombokStderrFilter.filter(absoluteRootB, Seq(absoluteRootA), index, stderr)

      filtered shouldBe stderr
    }

    "drop records whose location is an inner class of a peer-root type" in {
      val stderr = Seq(
        s"$absUserReport:19: error: cannot find symbol",
        "        UserRecord.Builder b = UserRecord.builder();",
        "                  ^",
        "  symbol:   class Builder",
        "  location: class com.example.UserRecord.Builder"
      )

      val filtered = DelombokStderrFilter.filter(absoluteRootB, Seq(absoluteRootA), index, stderr)

      filtered shouldBe empty
    }

    "drop records whose location is the `variable v of type C` form referencing a peer root type" in {
      val stderr = Seq(
        s"$absUserReport:20: error: cannot find symbol",
        "        String name = subject.getName();",
        "                             ^",
        "  symbol:   method getName()",
        "  location: variable subject of type com.example.UserRecord"
      )

      val filtered = DelombokStderrFilter.filter(absoluteRootB, Seq(absoluteRootA), index, stderr)

      filtered shouldBe empty
    }

    "keep records when the location FQN is declared in both current and peer roots (fail-open)" in {
      // Two file infos declaring the SAME FQN `com.example.Shared` from two different roots. When delomboking
      // root-b, an error whose location is `com.example.Shared` could originate from either root — keep it.
      val sharedRelativeA: Path = relativeRootA.resolve("com/example/Shared.java")
      val sharedRelativeB: Path = relativeRootB.resolve("com/example/Shared.java")
      val absSharedInB: Path    = absoluteRootB.resolve("com/example/Shared.java")
      val collidingFileInfo = List(
        SourceParser.FileInfo(sharedRelativeA, Some("com.example"), usesLombok = true),
        SourceParser.FileInfo(sharedRelativeB, Some("com.example"), usesLombok = true)
      )
      val collidingIndex = FqnIndex.build(inputPath, collidingFileInfo, packageRoots)

      val stderr = Seq(
        s"$absSharedInB:19: error: cannot find symbol",
        "        doSomething();",
        "        ^",
        "  symbol:   method doSomething()",
        "  location: class com.example.Shared"
      )

      val filtered = DelombokStderrFilter.filter(absoluteRootB, Seq(absoluteRootA), collidingIndex, stderr)

      filtered shouldBe stderr
    }

    "parse Windows-style diagnostic headers and drop peer-root records" in {
      // Path.of/Paths.get with a Windows drive-letter string only produces a proper absolute path on
      // Windows; on POSIX it becomes a relative fragment. Skip on non-Windows platforms.
      assume(File.separatorChar == '\\')
      // Synthetic setup with drive-letter absolute roots. The filter should parse the header and route the
      // location FQN through the index to conclude the record is a peer-root false positive.
      val winInput: Path        = Paths.get("C:\\repo\\proj").toAbsolutePath.normalize()
      val winRelRootA: Path     = Path.of("src/main/java")
      val winRelRootB: Path     = Path.of("src/main/java-report")
      val winAbsRootA: Path     = winInput.resolve(winRelRootA).toAbsolutePath.normalize()
      val winAbsRootB: Path     = winInput.resolve(winRelRootB).toAbsolutePath.normalize()
      val winRecordRel: Path    = winRelRootA.resolve("com/example/UserRecord.java")
      val winReportRel: Path    = winRelRootB.resolve("com/example/report/UserReport.java")
      val winAbsReport: Path    = winAbsRootB.resolve("com/example/report/UserReport.java")
      val winFileInfo = List(
        SourceParser.FileInfo(winRecordRel, Some("com.example"), usesLombok = true),
        SourceParser.FileInfo(winReportRel, Some("com.example.report"), usesLombok = true)
      )
      val winIndex = FqnIndex.build(winInput, winFileInfo, List(winRelRootA, winRelRootB))

      val stderr = Seq(
        s"$winAbsReport:19: error: cannot find symbol",
        "        UserRecord u = UserRecord.builder().build();",
        "                                 ^",
        "  symbol:   method builder()",
        "  location: class com.example.UserRecord"
      )

      val filtered = DelombokStderrFilter.filter(winAbsRootB, Seq(winAbsRootA), winIndex, stderr)

      filtered shouldBe empty
    }
  }
}
