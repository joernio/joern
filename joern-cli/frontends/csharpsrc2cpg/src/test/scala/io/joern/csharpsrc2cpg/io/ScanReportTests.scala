package io.joern.csharpsrc2cpg.io

import io.joern.csharpsrc2cpg.{Config, CSharpSrc2Cpg}
import io.joern.x2cpg.report.{Category, ScanReport, Severity}
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}

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
}
