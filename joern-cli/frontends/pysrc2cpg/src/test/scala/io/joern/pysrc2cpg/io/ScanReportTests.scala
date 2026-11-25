package io.joern.pysrc2cpg.io

import io.joern.pysrc2cpg.{Py2CpgOnFileSystem, Py2CpgOnFileSystemConfig}
import io.joern.x2cpg.report.{Category, ScanReport, Severity}
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}

class ScanReportTests extends AnyWordSpec with Matchers {

  "pysrc2cpg" should {

    "report a file with a syntax error as a problem" in {
      FileUtil.usingTemporaryDirectory("pysrc2cpgScanReport") { dir =>
        Files.writeString(dir / "good.py", "x = 1\n")
        Files.createDirectories(dir / "sub")
        Files.writeString(dir / "sub" / "broken.py", "def f(:\n  return 1\n")

        val report = new ScanReport()
        new Py2CpgOnFileSystem().createCpg(Py2CpgOnFileSystemConfig().withInputPath(dir.toString), report).get.close()

        val problems = report.allProblems.filter(_.file.contains(Paths.get("sub", "broken.py").toString))
        problems should not be empty
        problems.foreach { problem =>
          problem.severity shouldBe Severity.Warning
          problem.category should (be(Category.FileParse) or be(Category.FileProcessing))
          problem.message should not include "broken.py"
        }
        report.allProblems.filter(_.file.exists(_.endsWith("good.py"))) shouldBe empty
      }
    }

    "list the processed files" in {
      FileUtil.usingTemporaryDirectory("pysrc2cpgScanReport") { dir =>
        Files.writeString(dir / "good.py", "x = 1\ny = 2\n")

        val report = new ScanReport()
        new Py2CpgOnFileSystem().createCpg(Py2CpgOnFileSystemConfig().withInputPath(dir.toString), report).get.close()

        report.fileEntries.map(entry => (entry.path, entry.loc, entry.parsed, entry.cpgGenerated)) shouldBe
          Seq(("good.py", Some(2), true, true))
      }
    }
  }
}
