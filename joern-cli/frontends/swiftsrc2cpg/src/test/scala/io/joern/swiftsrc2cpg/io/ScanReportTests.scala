package io.joern.swiftsrc2cpg.io

import io.joern.swiftsrc2cpg.{Config, SwiftSrc2Cpg}
import io.joern.x2cpg.report.{Category, ScanReport}
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Files

class ScanReportTests extends AnyWordSpec with Matchers {

  "swiftsrc2cpg" should {

    // swiftastgen recovers from syntax errors, so there is no reliable way to make it skip a file
    "not report files that can be parsed" in {
      FileUtil.usingTemporaryDirectory("swiftsrc2cpgScanReport") { dir =>
        Files.writeString(dir / "good.swift", "func good() {}\n")

        val report = new ScanReport()
        new SwiftSrc2Cpg().createCpg(Config().withInputPath(dir.toString), report).get.close()

        report.allProblems shouldBe empty
        report.fileEntries.map(entry => (entry.path, entry.parsed, entry.cpgGenerated)) shouldBe
          Seq(("good.swift", true, true))
      }
    }

    "report a missing Swift package as a problem if type information was requested" in {
      FileUtil.usingTemporaryDirectory("swiftsrc2cpgScanReport") { dir =>
        Files.writeString(dir / "good.swift", "func good() {}\n")

        val report = new ScanReport()
        new SwiftSrc2Cpg().createCpg(Config().withInputPath(dir.toString).withSwiftBuild(true), report).get.close()

        val messages = report.allProblems.map(_.message)
        messages.exists(_.startsWith("Package.swift not found")) shouldBe true
        messages.exists(_.startsWith("No valid Swift environment found")) shouldBe true
        report.allProblems.foreach(_.category shouldBe Category.ExternalTool)
      }
    }
  }
}
