package io.joern.jimple2cpg.util

import io.joern.jimple2cpg.util.ProgramHandlingUtil.extractClassesInPackageLayout
import io.joern.x2cpg.report.{Category, ScanReport, Severity}
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Files

class ProgramHandlingProblemsTests extends AnyWordSpec with Matchers {

  "extracting class files" should {

    "report a class file that cannot be read" in {
      FileUtil.usingTemporaryDirectory("jimple2cpgProblems") { dir =>
        val src  = Files.createDirectories(dir / "src")
        val dest = Files.createDirectories(dir / "dest")
        val file = src / "Broken.class"
        Files.writeString(file, "this is not a class file")

        val report = new ScanReport()
        val result = extractClassesInPackageLayout(
          src,
          dest,
          isClass = entry => entry.extension.contains(".class"),
          isArchive = entry => entry.isZipFile,
          isConfigFile = entry => entry.isConfigFile,
          recurse = false,
          depth = 0,
          report
        )

        result shouldBe empty
        val problems = report.allProblems
        problems.map(_.category).distinct shouldBe Seq(Category.FileProcessing)
        problems.map(_.severity) should contain allOf (Severity.Error, Severity.Warning)
        problems.flatMap(_.file).distinct shouldBe Seq(file.toAbsolutePath.toString)
        problems.foreach(_.message should not include "Broken.class")
      }
    }
  }
}
