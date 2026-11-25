package io.joern.abap2cpg.io

import io.joern.abap2cpg.{Abap2Cpg, Config}
import io.joern.x2cpg.report.{Category, ScanReport, Severity}
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ScanReportTests extends AnyWordSpec with Matchers {

  "abap2cpg" should {

    "report an input without ABAP files as a problem" in {
      FileUtil.usingTemporaryDirectory("abap2cpgScanReport") { dir =>
        val report = new ScanReport()
        new Abap2Cpg().createCpg(Config().withInputPath(dir.toString), report).get.close()

        val problem = report.allProblems.find(_.message.contains("No ABAP files were parsed")).getOrElse(fail("none"))
        problem.severity shouldBe Severity.Warning
        problem.category shouldBe Category.Other
      }
    }
  }
}
