package io.joern.jssrc2cpg.io

import io.joern.jssrc2cpg.{Config, JsSrc2Cpg}
import io.joern.x2cpg.report.{Category, ReportMode, ScanReport, Severity}
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Files

class ScanReportTests extends AnyWordSpec with Matchers {

  private def withProjectWithBrokenFile[T](f: (java.nio.file.Path, java.nio.file.Path) => T): T = {
    FileUtil.usingTemporaryDirectory("jssrc2cpgScanReport") { dir =>
      val src = dir / "src"
      Files.createDirectories(src)
      Files.writeString(src / "good.js", """console.log("good");""")
      Files.writeString(src / "broken.js", """console.log("broken""")
      f(src, dir)
    }
  }

  "jssrc2cpg" should {

    "report a file that cannot be parsed as a problem" in withProjectWithBrokenFile { (src, _) =>
      val report = new ScanReport()
      new JsSrc2Cpg().createCpg(Config(tsTypes = false).withInputPath(src.toString), report).get.close()

      val problem = report.allProblems.find(_.file.contains("broken.js")).getOrElse(fail("no problem for broken.js"))
      problem.severity shouldBe Severity.Warning
      problem.category shouldBe Category.FileParse
      problem.message should startWith("Failed to parse")
      problem.message should not include "broken.js"

      val entries = report.fileEntries.map(entry => entry.path -> entry.cpgGenerated).toMap
      entries.get("good.js") shouldBe Some(true)
      entries.get("broken.js") shouldBe Some(false)
      report.fileEntries.find(_.path == "broken.js").flatMap(_.loc) shouldBe Some(1)
    }

    "write the problem to the JSON report of the run" in withProjectWithBrokenFile { (src, dir) =>
      val reportFile = dir / "report.json"
      val config     = Config(tsTypes = false)
        .withInputPath(src.toString)
        .withReportMode(ReportMode.Json(Some(reportFile.toString)))
      new JsSrc2Cpg().run(config)

      val json = ujson.read(Files.readString(reportFile))
      json("status").str shouldBe "Completed"
      json("problems").arr.map(_("file").str).toSeq should contain("broken.js")
      json("summary")("warnings").num should be >= 1.0
    }
  }
}
