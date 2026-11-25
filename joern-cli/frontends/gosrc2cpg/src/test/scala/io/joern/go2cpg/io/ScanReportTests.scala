package io.joern.go2cpg.io

import io.joern.gosrc2cpg.{Config, GoSrc2Cpg}
import io.joern.x2cpg.report.{Category, ScanReport, Severity}
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}

class ScanReportTests extends AnyWordSpec with Matchers {

  "gosrc2cpg" should {

    "report a file that cannot be parsed as a problem" in {
      FileUtil.usingTemporaryDirectory("gosrc2cpgScanReport") { dir =>
        Files.writeString(dir / "go.mod", "module joern.io/sample\n\ngo 1.18\n")
        Files.writeString(dir / "good.go", "package main\n\nfunc main() {}\n")
        Files.createDirectories(dir / "sub")
        Files.writeString(dir / "sub" / "broken.go", "package sub\n\nfunc broken( {\n")

        val report = new ScanReport()
        new GoSrc2Cpg().createCpg(Config().withInputPath(dir.toString), report).get.close()

        val problems = report.allProblems.filter(_.file.contains(Paths.get("sub", "broken.go").toString))
        problems should not be empty
        problems.foreach { problem =>
          problem.severity shouldBe Severity.Warning
          problem.category shouldBe Category.FileParse
          problem.message should not include "broken.go"
        }
        report.allProblems.filter(_.file.exists(_.endsWith("good.go"))) shouldBe empty
      }
    }
  }
}
