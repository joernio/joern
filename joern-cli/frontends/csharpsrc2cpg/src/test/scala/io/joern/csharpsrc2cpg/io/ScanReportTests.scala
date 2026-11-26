package io.joern.csharpsrc2cpg.io

import io.joern.csharpsrc2cpg.utils.DotNetAstGenRunner
import io.joern.csharpsrc2cpg.{Config, CSharpSrc2Cpg}
import io.joern.x2cpg.report.{Category, ScanReport, Severity}
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Path, Paths}

class ScanReportTests extends AnyWordSpec with Matchers {

  "csharpsrc2cpg" should {

    "report a file that cannot be parsed as a problem" in {
      FileUtil.usingTemporaryDirectory("csharpsrc2cpgScanReport") { dir =>
        Files.writeString(dir / "Good.cs", "class Good { void F() {} }")
        Files.createDirectories(dir / "sub")
        Files.writeString(dir / "sub" / "Broken.cs", "class Broken { void F( {")

        val report = new ScanReport()
        new CSharpSrc2Cpg().createCpg(Config().withInputPath(dir.toString), report).get.close()

        val problems = report.allProblems.filter(_.file.contains(Paths.get("sub", "Broken.cs").toString))
        problems should not be empty
        problems.foreach { problem =>
          problem.severity shouldBe Severity.Warning
          problem.category shouldBe Category.FileParse
          problem.message should not include "Broken.cs"
        }
        report.allProblems.filter(_.file.exists(_.endsWith("Good.cs"))) shouldBe empty
      }
    }
  }

  // dotnetastgen parses in parallel: the diagnostics of a file do not follow its `Parsing file` line
  "the diagnostics of dotnetastgen" should {
    val in     = Paths.get("input")
    val prefix = "DotNetAstGen.Program[0]"

    def problemsOf(lines: String*): Seq[(Option[String], String)] = {
      val report = new ScanReport()
      new DotNetAstGenRunner(Config().withInputPath(in.toString), report).skippedFiles(in, lines.toList)
      report.allProblems.map(problem => (problem.file, problem.message))
    }

    def path(file: String*): String = in.toAbsolutePath.resolve(Paths.get(file.head, file.tail*)).toString

    val diagnostics =
      Seq(s"fail: $prefix (1,24): error CS1026: ) expected", s"fail: $prefix (1,25): error CS1513: } expected")

    "be reported for the only file that failed, no matter which file started to parse last" in {
      val broken = path("sub", "Broken.cs")
      val good   = path("Good.cs")
      val failed = s"fail: $prefix Error(s) encountered while parsing: $broken"
      val orders = Seq(
        Seq(s"info: $prefix Parsing file: $broken", s"info: $prefix Parsing file: $good"),
        Seq(s"info: $prefix Parsing file: $good", s"info: $prefix Parsing file: $broken")
      )
      orders.foreach { parsing =>
        val problems = problemsOf((parsing ++ diagnostics ++ Seq(s"info: $prefix Successfully parsed: $good", failed))*)
        problems.map(_._1) shouldBe Seq(Some(Paths.get("sub", "Broken.cs").toString))
        problems.head._2 should include("error CS1026")
        problems.head._2 should include("error CS1513")
      }
    }

    "not be assigned to a file if several files failed" in {
      val first    = path("A.cs")
      val second   = path("B.cs")
      val problems = problemsOf(
        (diagnostics ++ Seq(
          s"fail: $prefix Error(s) encountered while parsing: $first",
          s"fail: $prefix Error(s) encountered while parsing: $second"
        ))*
      )
      problems shouldBe Seq((Some("A.cs"), "Failed to parse"), (Some("B.cs"), "Failed to parse"))
    }

    "be reported for the file they name" in {
      val odd = path("Odd.cs")
      problemsOf(s"warn: $prefix $odd does not contain any code, skipping...") shouldBe
        Seq((Some("Odd.cs"), "Failed to parse:\n - does not contain any code"))
    }

    "not be reported for files that were parsed" in {
      val good = path("Good.cs")
      problemsOf(s"info: $prefix Parsing file: $good", s"info: $prefix Successfully parsed: $good") shouldBe empty
    }
  }
}
