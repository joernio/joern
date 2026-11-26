package io.joern.rubysrc2cpg.io

import io.joern.rubysrc2cpg.{Config, RubySrc2Cpg}
import io.joern.x2cpg.report.{Category, ScanReport, Severity}
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}

class ScanReportTests extends AnyWordSpec with Matchers {

  "rubysrc2cpg" should {

    "report a file that cannot be parsed as a problem" in {
      FileUtil.usingTemporaryDirectory("rubysrc2cpgScanReport") { dir =>
        Files.writeString(dir / "good.rb", "puts 1\n")
        Files.createDirectories(dir / "sub")
        Files.writeString(dir / "sub" / "broken.rb", "def f(\n  puts 1\nend end end\n")

        val frontend = new RubySrc2Cpg()
        try {
          val report = new ScanReport()
          frontend.createCpg(Config().withInputPath(dir.toString), report).get.close()

          val problems = report.allProblems.filter(_.file.contains(Paths.get("sub", "broken.rb").toString))
          problems should not be empty
          problems.foreach { problem =>
            problem.severity shouldBe Severity.Warning
            problem.category shouldBe Category.FileParse
            problem.message should not include "broken.rb"
          }
          report.allProblems.filter(_.file.exists(_.endsWith("good.rb"))) shouldBe empty

          val files = report.fileEntries.map(entry => entry.path -> entry).toMap
          files.keySet shouldBe Set("good.rb", Paths.get("sub", "broken.rb").toString)
          files("good.rb").loc shouldBe Some(1)
          files("good.rb").parsed shouldBe true
          files("good.rb").cpgGenerated shouldBe true
          files(Paths.get("sub", "broken.rb").toString).parsed shouldBe false
          files(Paths.get("sub", "broken.rb").toString).cpgGenerated shouldBe false
        } finally {
          frontend.close()
        }
      }
    }
  }
}
