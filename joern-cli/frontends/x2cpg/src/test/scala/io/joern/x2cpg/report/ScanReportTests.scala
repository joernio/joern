package io.joern.x2cpg.report

import io.joern.x2cpg.{X2Cpg, X2CpgConfig, X2CpgFrontend}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scopt.OParser

import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.{Callable, Executors}
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}

object ScanReportTests {

  final case class StubConfig(override val genericConfig: X2CpgConfig.GenericConfig = X2CpgConfig.GenericConfig())
      extends X2CpgConfig[StubConfig] {
    override def withGenericConfig(value: X2CpgConfig.GenericConfig): StubConfig = copy(genericConfig = value)
  }

  /** A frontend that reports through the report handed to it by `run`. */
  final class ReportingFrontend(create: ScanReport => Try[Cpg]) extends X2CpgFrontend {
    override type ConfigType = StubConfig
    override val defaultConfig: StubConfig = StubConfig()

    override def createCpg(config: StubConfig): Try[Cpg] = create(new ScanReport())

    override def createCpg(config: StubConfig, report: ScanReport): Try[Cpg] = create(report)
  }

  /** A frontend that has not been migrated to report problems. */
  final class PlainFrontend extends X2CpgFrontend {
    override type ConfigType = StubConfig
    override val defaultConfig: StubConfig = StubConfig()

    override def createCpg(config: StubConfig): Try[Cpg] = Success(X2Cpg.newEmptyCpg(None))
  }

  private val metadata = ScanReport.Metadata("stub", "local build", "/in", Some("/out/cpg.bin"), RunStatus.Completed)

  private def parse(args: String*): Option[StubConfig] =
    X2Cpg.parseCommandLine(args.toArray, OParser.builder[StubConfig].programName("stub"), StubConfig())

  private def readJson(path: Path): ujson.Value = ujson.read(Files.readString(path))
}

class ScanReportTests extends AnyWordSpec with Matchers {
  import ScanReportTests.*

  private def fill(report: ScanReport, files: Seq[String]): Unit = {
    files.foreach { file => report.addReportInfo(file, 10, parsed = true); report.updateReport(file, true, 2000000) }
  }

  "ScanReport" should {

    "record problems with their category and the description of the cause" in {
      val report = new ScanReport()
      report.error(Category.FileProcessing, "boom", Some("a.c"), Some(new IllegalStateException("bad state")))
      report.warn(Category.DependencyResolution, "no deps")

      report.allProblems shouldBe Seq(
        Problem(Severity.Warning, Category.DependencyResolution, "no deps"),
        Problem(
          Severity.Error,
          Category.FileProcessing,
          "boom",
          Some("a.c"),
          Some(ExceptionInfo("java.lang.IllegalStateException", Some("bad state")))
        )
      )
    }

    "not report the exception message if there is none" in {
      val report = new ScanReport()
      report.warn(Category.Other, "oops", cause = Some(new StackOverflowError()))
      report.allProblems.map(_.exception) shouldBe Seq(Some(ExceptionInfo("java.lang.StackOverflowError")))
    }

    "produce the same JSON independent of the order in which things were reported" in {
      val first = new ScanReport()
      first.warn(Category.FileParse, "x", Some("b.c"))
      first.warn(Category.FileParse, "y", Some("a.c"))
      first.warn(Category.DependencyResolution, "z")
      fill(first, Seq("b.c", "a.c"))

      val second = new ScanReport()
      fill(second, Seq("a.c", "b.c"))
      second.warn(Category.DependencyResolution, "z")
      second.warn(Category.FileParse, "y", Some("a.c"))
      second.warn(Category.FileParse, "x", Some("b.c"))

      ujson.write(first.toJson(metadata)) shouldBe ujson.write(second.toJson(metadata))
      first.fileEntries.map(_.path) shouldBe Seq("a.c", "b.c")
    }

    "be safe to use from many threads" in {
      val report   = new ScanReport()
      val executor = Executors.newFixedThreadPool(8)
      try {
        val tasks = (0 until 200).map { i =>
          new Callable[Unit] {
            override def call(): Unit = {
              report.warn(Category.FileParse, s"problem $i", Some(s"file$i.c"))
              report.addReportInfo(s"file$i.c", i)
            }
          }
        }
        executor.invokeAll(tasks.asJava).asScala.foreach(_.get())
      } finally {
        executor.shutdown()
      }
      report.allProblems.size shouldBe 200
      report.fileEntries.size shouldBe 200
    }

    "list the files of file problems that were not registered" in {
      val report = new ScanReport()
      report.addReportInfo("registered.c", 10, parsed = true)
      report.updateReport("registered.c", cpg = true, 1000)
      report.warn(Category.FileParse, "cannot be parsed", Some("unparsable.c"))
      report.warn(Category.FileProcessing, "cannot be processed", Some("unprocessable.c"))
      report.warn(Category.FileProcessing, "cannot be processed twice", Some("unprocessable.c"))
      report.warn(Category.FileParse, "parse", Some("both.c"))
      report.warn(Category.FileProcessing, "process", Some("both.c"))
      report.warn(Category.FileParse, "registered anyway", Some("registered.c"))
      report.warn(Category.DependencyResolution, "not a source file", Some("lib.jar"))
      report.warn(Category.Other, "not about a file")

      report.fileEntries.map(entry => (entry.path, entry.loc, entry.parsed, entry.cpgGenerated)) shouldBe Seq(
        ("both.c", None, false, false),
        ("registered.c", Some(10), true, true),
        ("unparsable.c", None, false, false),
        ("unprocessable.c", None, true, false)
      )
      report.toJson(metadata)("summary")("files").num shouldBe 4
    }

    "not list a file that could not be processed as a file that got a CPG" in {
      val report = new ScanReport()
      report.addReportInfo("recovered.c", 10, parsed = true)
      report.updateReport("recovered.c", cpg = true, 1000)
      report.warn(Category.FileProcessing, "recovered with an empty AST", Some("recovered.c"))

      report.fileEntries.map(entry => (entry.path, entry.parsed, entry.cpgGenerated)) shouldBe
        Seq(("recovered.c", true, false))
    }

    "leave out the message of an exception that has none in the JSON" in {
      val report = new ScanReport()
      report.warn(Category.Other, "oops", cause = Some(new StackOverflowError()))
      val exception = report.toJson(metadata)("problems")(0)("exception")
      exception("type").str shouldBe "java.lang.StackOverflowError"
      exception.obj.contains("message") shouldBe false
    }

    "leave out what is not available in the JSON" in {
      val report = new ScanReport()
      report.updateReport("unreadable.js", cpg = false, duration = 0)
      report.warn(Category.Other, "no file and no exception")

      val json = report.toJson(metadata.copy(cpgPath = None))
      json.obj.contains("cpgPath") shouldBe false

      val file = json("files")(0)
      file("path").str shouldBe "unreadable.js"
      file("parsed").bool shouldBe false
      file.obj.keySet should contain noneOf ("loc", "durationMs")

      val problem = json("problems")(0)
      problem("message").str shouldBe "no file and no exception"
      problem.obj.keySet should contain noneOf ("file", "exception")
    }

    "keep what is available in the JSON" in {
      val report = new ScanReport()
      report.addReportInfo("a.c", 3, parsed = true)
      report.updateReport("a.c", cpg = true, 1500000)
      report.warn(Category.FileProcessing, "failed", Some("a.c"), Some(new RuntimeException("kaputt")))

      val json = report.toJson(metadata)
      json("cpgPath").str shouldBe "/out/cpg.bin"
      json("files")(0)("loc").num shouldBe 3
      json("files")(0)("durationMs").num shouldBe 1.5
      json("problems")(0)("file").str shouldBe "a.c"
      json("problems")(0)("exception")("type").str shouldBe "java.lang.RuntimeException"
      json("problems")(0)("exception")("message").str shouldBe "kaputt"
    }

    "write a JSON file with metadata, summary, problems and files" in {
      FileUtil.usingTemporaryDirectory("scanReport") { dir =>
        val report = new ScanReport()
        report.addReportInfo("a.c", 12, parsed = true)
        report.updateReport("a.c", cpg = false, 5000000)
        report.error(Category.FileProcessing, "failed", Some("a.c"), Some(new RuntimeException("kaputt")))
        report.warn(Category.DependencyResolution, "no network")

        val target = dir / "nested" / "report.json"
        report.writeJson(target, metadata)

        val json = readJson(target)
        json("schemaVersion").num shouldBe 1
        json("frontend").str shouldBe "stub"
        json("status").str shouldBe "Completed"
        json("cpgPath").str shouldBe "/out/cpg.bin"
        json("summary")("files").num shouldBe 1
        json("summary")("loc").num shouldBe 12
        json("summary")("errors").num shouldBe 1
        json("summary")("warnings").num shouldBe 1
        json("problems").arr.map(_("category").str).toSeq shouldBe Seq("DependencyResolution", "FileProcessing")
        json("problems")(1)("exception")("type").str shouldBe "java.lang.RuntimeException"
        json("problems")(1)("exception")("message").str shouldBe "kaputt"
        json("problems")(0).obj.contains("file") shouldBe false
        json("files")(0)("durationMs").num shouldBe 5.0
        Files.list(target.getParent).iterator().asScala.map(_.getFileName.toString).toSeq shouldBe Seq("report.json")
      }
    }

    "write the file with the default permissions of new files, like the CPG" in {
      assume(!scala.util.Properties.isWin, "no POSIX file permissions")
      FileUtil.usingTemporaryDirectory("scanReport") { dir =>
        val reference = Files.createFile(dir / "reference")
        val target    = dir / "report.json"
        new ScanReport().writeJson(target, metadata)
        Files.getPosixFilePermissions(target) shouldBe Files.getPosixFilePermissions(reference)
      }
    }

    "replace an existing report" in {
      FileUtil.usingTemporaryDirectory("scanReport") { dir =>
        val target = dir / "report.json"
        Files.writeString(target, "stale")
        new ScanReport().writeJson(target, metadata)
        readJson(target)("summary")("files").num shouldBe 0
      }
    }
  }

  "ScanReport.defaultJsonPath" should {
    "be located next to the CPG" in {
      ScanReport.defaultJsonPath("/some/dir/my.cpg") shouldBe Paths.get("/some/dir/my.cpg.report.json")
    }

    "fall back to the default CPG name if the CPG is not persisted" in {
      ScanReport.defaultJsonPath("").getFileName.toString shouldBe "cpg.bin.report.json"
    }
  }

  "the command line" should {
    "only log the report by default" in {
      parse("in").map(_.reportMode) shouldBe Some(ReportMode.Log)
    }

    "write the JSON next to the CPG with --report-json" in {
      parse("in", "--report-json").map(_.reportMode) shouldBe Some(ReportMode.Json(None))
    }

    "accept the positional input after --report-json" in {
      val config = parse("--report-json", "in").get
      config.reportMode shouldBe ReportMode.Json(None)
      config.inputPath should endWith("in")
    }

    "write the JSON to the given file with --report-json-file" in {
      parse("in", "--report-json-file", "out.json").map(_.reportMode) shouldBe Some(ReportMode.Json(Some("out.json")))
    }

    "keep the given file if --report-json is added" in {
      parse("in", "--report-json-file", "out.json", "--report-json").map(_.reportMode) shouldBe
        Some(ReportMode.Json(Some("out.json")))
    }
  }

  "X2CpgFrontend.run" should {

    "write the JSON report of the run" in {
      FileUtil.usingTemporaryDirectory("scanReport") { dir =>
        val frontend = new ReportingFrontend({ report =>
          report.warn(Category.DependencyResolution, "no network")
          Success(X2Cpg.newEmptyCpg(None))
        })
        val target = dir / "report.json"
        frontend.run(StubConfig().withInputPath("in").withReportMode(ReportMode.Json(Some(target.toString))))

        val json = readJson(target)
        json("status").str shouldBe "Completed"
        json("frontend").str shouldBe "ReportingFrontend"
        json("problems")(0)("message").str shouldBe "no network"
      }
    }

    "write the JSON report next to the CPG by default" in {
      FileUtil.usingTemporaryDirectory("scanReport") { dir =>
        val frontend = new ReportingFrontend(_ => Success(X2Cpg.newEmptyCpg(None)))
        val cpgPath  = dir / "my.cpg"
        frontend.run(StubConfig().withOutputPath(cpgPath.toString).withReportMode(ReportMode.Json()))

        val json = readJson(dir / "my.cpg.report.json")
        json("cpgPath").str shouldBe cpgPath.toString
      }
    }

    "not write anything unless requested" in {
      FileUtil.usingTemporaryDirectory("scanReport") { dir =>
        val frontend = new ReportingFrontend(_ => Success(X2Cpg.newEmptyCpg(None)))
        frontend.run(StubConfig().withOutputPath((dir / "my.cpg").toString))
        Files.list(dir).iterator().asScala.toSeq shouldBe empty
      }
    }

    "still write the report if the run fails and rethrow the failure" in {
      FileUtil.usingTemporaryDirectory("scanReport") { dir =>
        val frontend = new ReportingFrontend({ report =>
          report.error(Category.FileParse, "broken", Some("a.c"))
          Failure(new IllegalStateException("fatal"))
        })
        val target = dir / "report.json"
        val config = StubConfig().withReportMode(ReportMode.Json(Some(target.toString)))

        the[IllegalStateException] thrownBy frontend.run(config) should have message "fatal"

        val json = readJson(target)
        json("status").str shouldBe "Failed"
        json("problems")(0)("file").str shouldBe "a.c"
      }
    }

    "use a fresh report for every run" in {
      FileUtil.usingTemporaryDirectory("scanReport") { dir =>
        val frontend = new ReportingFrontend({ report =>
          report.warn(Category.Other, "once")
          Success(X2Cpg.newEmptyCpg(None))
        })
        val target = dir / "report.json"
        val config = StubConfig().withReportMode(ReportMode.Json(Some(target.toString)))
        frontend.run(config)
        frontend.run(config)
        readJson(target)("problems").arr.size shouldBe 1
      }
    }

    "write an empty report for a frontend that does not report" in {
      FileUtil.usingTemporaryDirectory("scanReport") { dir =>
        val target = dir / "report.json"
        new PlainFrontend().run(StubConfig().withReportMode(ReportMode.Json(Some(target.toString))))
        readJson(target)("problems").arr shouldBe empty
      }
    }

    "not fail the run if the report cannot be written" in {
      FileUtil.usingTemporaryDirectory("scanReport") { dir =>
        val blocker = dir / "file"
        Files.writeString(blocker, "not a directory")
        val config = StubConfig().withReportMode(ReportMode.Json(Some((blocker / "report.json").toString)))
        noException should be thrownBy new PlainFrontend().run(config)
      }
    }
  }
}
