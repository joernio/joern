package io.joern.javasrc2cpg.io

import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.joern.x2cpg.report.{Category, ScanReport, Severity}
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}

class ScanReportTests extends AnyWordSpec with Matchers {

  "javasrc2cpg" should {

    "report a file that cannot be parsed as a problem" in {
      FileUtil.usingTemporaryDirectory("javasrc2cpgScanReport") { dir =>
        Files.writeString(dir / "Good.java", "public class Good { void f() {} }")
        Files.createDirectories(dir / "sub")
        Files.writeString(dir / "sub" / "Broken.java", "public class Broken { void f( { }")

        val report = new ScanReport()
        new JavaSrc2Cpg().createCpg(Config().withInputPath(dir.toString), report).get.close()

        val problems = report.allProblems.filter(_.file.contains(Paths.get("sub", "Broken.java").toString))
        problems should not be empty
        problems.foreach { problem =>
          problem.severity shouldBe Severity.Warning
          problem.category shouldBe Category.FileParse
          problem.message should not include "Broken.java"
        }
        report.allProblems.filter(_.file.exists(_.endsWith("Good.java"))) shouldBe empty
      }
    }
  }
}
