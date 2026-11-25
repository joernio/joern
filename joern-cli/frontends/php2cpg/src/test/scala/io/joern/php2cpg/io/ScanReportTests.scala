package io.joern.php2cpg.io

import io.joern.php2cpg.passes.DependencyPass
import io.joern.php2cpg.{Config, Php2Cpg}
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.joern.x2cpg.report.{Category, ScanReport, Severity}
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}

class ScanReportTests extends AnyWordSpec with Matchers {

  "php2cpg" should {

    "not report files that can be parsed" in {
      FileUtil.usingTemporaryDirectory("php2cpgScanReport") { dir =>
        Files.writeString(dir / "good.php", "<?php\necho 1;\n")

        val report = new ScanReport()
        new Php2Cpg().createCpg(Config().withInputPath(dir.toString), report).get.close()

        report.allProblems shouldBe empty
      }
    }

    "report a composer file that cannot be read as a problem" in {
      FileUtil.usingTemporaryDirectory("php2cpgScanReport") { dir =>
        Files.createDirectories(dir / "sub")
        val composer = dir / "sub" / "composer.json"
        Files.writeString(composer, "{ not json")

        val report = new ScanReport()
        val cpg    = X2Cpg.newEmptyCpg()
        new MetaDataPass(cpg, Languages.PHP, dir.toString).createAndApply()
        new DependencyPass(cpg, List(composer.toString), report).createAndApply()
        cpg.close()

        val problem = report.allProblems.head
        problem.severity shouldBe Severity.Error
        problem.category shouldBe Category.FileParse
        problem.file shouldBe Some(Paths.get("sub", "composer.json").toString)
        problem.exception should not be empty
      }
    }
  }
}
