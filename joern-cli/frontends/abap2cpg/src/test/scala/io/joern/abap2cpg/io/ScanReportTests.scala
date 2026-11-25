package io.joern.abap2cpg.io

import io.joern.abap2cpg.{Abap2Cpg, Config}
import io.joern.x2cpg.report.{Category, ScanReport, Severity}
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Files

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

    "list the processed files" in {
      FileUtil.usingTemporaryDirectory("abap2cpgScanReport") { dir =>
        Files.writeString(dir / "zhello.prog.abap", "REPORT zhello.\nWRITE 'hello'.\n")

        val report = new ScanReport()
        new Abap2Cpg().createCpg(Config().withInputPath(dir.toString), report).get.close()

        report.fileEntries.map(entry => (entry.path, entry.loc, entry.parsed, entry.cpgGenerated)) shouldBe
          Seq(("zhello.prog.abap", Some(2), true, true))
      }
    }
  }
}
