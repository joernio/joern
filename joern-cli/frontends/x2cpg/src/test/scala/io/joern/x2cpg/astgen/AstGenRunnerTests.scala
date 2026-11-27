package io.joern.x2cpg.astgen

import io.joern.x2cpg.X2CpgConfig
import io.joern.x2cpg.astgen.AstGenRunner.{AstGenFailedException, AstGenProgramMetaData}
import io.joern.x2cpg.report.{Category, ExceptionInfo, ProblemSink, ScanReport, Severity}
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*
import org.scalatest.LoneElement
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.slf4j.LoggerFactory

import java.nio.file.{Files, Path, Paths}
import java.util.regex.Pattern
import scala.util.{Failure, Success, Try}

object AstGenRunnerTests {

  final case class StubConfig(override val genericConfig: X2CpgConfig.GenericConfig = X2CpgConfig.GenericConfig())
      extends X2CpgConfig[StubConfig] {
    override def withGenericConfig(value: X2CpgConfig.GenericConfig): StubConfig = copy(genericConfig = value)
  }

  private object StubMetaData extends AstGenProgramMetaData(name = "stubastgen", configPrefix = "stub")

  /** An AST generator that does not run any process. It writes the given JSON files to the output directory and then
    * returns the given outcome.
    */
  final class StubAstGenRunner(
    config: StubConfig,
    jsonFiles: Seq[String],
    outcome: Try[Seq[String]],
    problemSink: ProblemSink = ProblemSink.LogOnly,
    legacyAbsoluteExcludeRegex: Boolean = true
  ) extends AstGenRunner(StubMetaData, config, problemSink) {

    private val logger = LoggerFactory.getLogger(getClass)

    override protected def allowLegacyAbsoluteExcludeRegex: Boolean = legacyAbsoluteExcludeRegex

    override protected def skippedFiles(in: Path, astGenOut: List[String]): List[String] =
      astGenOut.collect {
        case line if line.startsWith("Skipped: ") =>
          val file = line.stripPrefix("Skipped: ")
          reportParseProblem(logger, file, "Failed to parse")
          file
      }

    override protected def runAstGenNative(
      in: String,
      out: Path,
      exclude: String,
      include: String
    ): Try[Seq[String]] = {
      jsonFiles.foreach { file =>
        Files.createDirectories((out / file).getParent)
        Files.writeString(out / file, "{}")
      }
      outcome
    }
  }
}

class AstGenRunnerTests extends AnyWordSpec with Matchers with LoneElement {
  import AstGenRunnerTests.*

  private def withRunner[T](
    jsonFiles: Seq[String],
    outcome: Try[Seq[String]],
    problems: ProblemSink = ProblemSink.LogOnly
  )(f: (StubAstGenRunner, Path) => T): T = {
    FileUtil.usingTemporaryDirectory("astgenRunnerIn") { inputDir =>
      FileUtil.usingTemporaryDirectory("astgenRunnerOut") { outputDir =>
        val config = StubConfig().withInputPath(inputDir.toString)
        f(new StubAstGenRunner(config, jsonFiles, outcome, problems), outputDir)
      }
    }
  }

  "AstGenRunner.execute" should {

    "report the parsed and skipped files of a successful run" in {
      withRunner(Seq("a.json"), Success(Seq("Skipped: b.js"))) { (runner, out) =>
        val result = runner.execute(out)
        result.parsedFiles shouldBe List((out / "a.json").toString)
        result.skippedFiles shouldBe List("b.js")
      }
    }

    "accept a successful run without any files, e.g., an input without parsable sources" in {
      withRunner(Seq.empty, Success(Seq.empty)) { (runner, out) =>
        val result = runner.execute(out)
        result.parsedFiles shouldBe empty
        result.skippedFiles shouldBe empty
      }
    }

    "accept a run that only skipped files, e.g., an input with just a syntax error" in {
      withRunner(Seq.empty, Success(Seq("Skipped: broken.js"))) { (runner, out) =>
        val result = runner.execute(out)
        result.parsedFiles shouldBe empty
        result.skippedFiles shouldBe List("broken.js")
      }
    }

    "continue with the files of a partially failed run" in {
      withRunner(Seq("a.json"), Failure(new RuntimeException("exit code 1"))) { (runner, out) =>
        val result = runner.execute(out)
        result.parsedFiles shouldBe List((out / "a.json").toString)
        result.skippedFiles shouldBe empty
      }
    }

    "report a partially failed run as a problem of the external tool" in {
      val report = new ScanReport()
      withRunner(Seq("a.json"), Failure(new RuntimeException("exit code 1")), report) { (runner, out) =>
        runner.execute(out)
      }
      val problem = report.allProblems.loneElement
      problem.severity shouldBe Severity.Warning
      problem.category shouldBe Category.ExternalTool
      problem.message should include("stubastgen")
      problem.exception shouldBe Some(ExceptionInfo("java.lang.RuntimeException", Some("exit code 1")))
    }

    "report the files that could not be parsed" in {
      val report = new ScanReport()
      withRunner(Seq("a.json"), Success(Seq("Skipped: rel/b.js")), report) { (runner, out) => runner.execute(out) }
      report.allProblems.map(problem => (problem.severity, problem.category, problem.file)) shouldBe
        Seq((Severity.Warning, Category.FileParse, Some("rel/b.js")))
    }

    "report files printed with an absolute path relative to the input path" in {
      FileUtil.usingTemporaryDirectory("astgenRunnerIn") { inputDir =>
        FileUtil.usingTemporaryDirectory("astgenRunnerOut") { outputDir =>
          val report = new ScanReport()
          val config = StubConfig().withInputPath(inputDir.toString)
          val output = Success(Seq(s"Skipped: ${inputDir / "sub" / "c.js"}"))
          new StubAstGenRunner(config, Seq("a.json"), output, report).execute(outputDir)
          report.allProblems.map(_.file) shouldBe Seq(Some(Paths.get("sub", "c.js").toString))
        }
      }
    }

    "not report anything if every file could be parsed" in {
      val report = new ScanReport()
      withRunner(Seq("a.json"), Success(Seq("Generated: a.json")), report) { (runner, out) => runner.execute(out) }
      report.allProblems shouldBe empty
    }

    "throw if the run failed without producing any output" in {
      val cause = new RuntimeException("exit code 1")
      withRunner(Seq.empty, Failure(cause)) { (runner, out) =>
        val exception = intercept[AstGenFailedException](runner.execute(out))
        exception.getCause shouldBe cause
        exception.getMessage should include("stubastgen")
      }
    }
  }

  "AstGenRunner.execute with --exclude-regex" should {

    /** Relative paths use the separator of the operating system, so regexes must accept both `/` and `\`. */
    val sep = "[/\\\\]"

    val sources = Seq("a.js", "sub/b.js", "sub/deep/c.js")

    /** Runs the stub on `sources` in a fresh input directory and returns the parsed ones, relative to it. */
    def parsedSources(
      regex: Path => String,
      report: ProblemSink = ProblemSink.LogOnly,
      legacyAbsoluteExcludeRegex: Boolean = true
    ): Seq[String] = {
      FileUtil.usingTemporaryDirectory("astgenRunnerIn") { inputDir =>
        FileUtil.usingTemporaryDirectory("astgenRunnerOut") { outputDir =>
          sources.foreach { source =>
            Files.createDirectories((inputDir / source).getParent)
            Files.writeString(inputDir / source, "")
          }
          val config = StubConfig().withInputPath(inputDir.toString).withIgnoredFilesRegex(regex(inputDir))
          val runner = new StubAstGenRunner(
            config,
            sources.map(_ + ".json"),
            Success(Seq.empty),
            report,
            legacyAbsoluteExcludeRegex
          )
          runner
            .execute(outputDir)
            .parsedFiles
            .map(file => outputDir.relativize(Paths.get(file)).toString.stripSuffix(".json"))
            .map(_.replace(java.io.File.separator, "/"))
            .sorted
        }
      }
    }

    def absolute(inputDir: Path): String = Pattern.quote(inputDir.toString)

    "exclude the files whose relative path fully matches" in {
      parsedSources(_ => "a\\.js") shouldBe Seq("sub/b.js", "sub/deep/c.js")
    }

    "exclude everything below a directory whose relative path fully matches" in {
      parsedSources(_ => "sub") shouldBe Seq("a.js")
      parsedSources(_ => s"sub$sep.*") shouldBe Seq("a.js")
    }

    "support lookaheads" in {
      parsedSources(_ => s"(?!sub${sep}deep$sep.*$$).*\\.js") shouldBe Seq("sub/deep/c.js")
    }

    "not exclude anything if the regex only matches a part of the relative path" in {
      parsedSources(_ => "ub") shouldBe sources.sorted
      parsedSources(_ => "b\\.js") shouldBe sources.sorted
    }

    "not match the suffix of the generated JSON files" in {
      parsedSources(_ => ".*json.*") shouldBe sources.sorted
      parsedSources(_ => ".*\\.js\\.json") shouldBe sources.sorted
      parsedSources(_ => "a\\.js\\.json") shouldBe sources.sorted
    }

    "be case-sensitive" in {
      parsedSources(_ => "A\\.JS") shouldBe sources.sorted
    }

    "not report anything if the regex matches the relative path" in {
      val report = new ScanReport()
      parsedSources(_ => s"sub$sep.*", report) shouldBe Seq("a.js")
      report.allProblems shouldBe empty
    }

    "still exclude if the regex only matches the absolute path, and report once that this is deprecated" in {
      val report = new ScanReport()
      parsedSources(inputDir => s"${absolute(inputDir)}$sep(sub$sep.*)", report) shouldBe Seq("a.js")
      val problem = report.allProblems.loneElement
      problem.severity shouldBe Severity.Warning
      problem.category shouldBe Category.Other
      problem.message should include("deprecated")
      problem.message should include("absolute")
    }

    "not exclude if the regex only matches the absolute path and that is not allowed" in {
      parsedSources(inputDir => s"${absolute(inputDir)}$sep.*", legacyAbsoluteExcludeRegex = false) shouldBe
        sources.sorted
    }
  }

}
