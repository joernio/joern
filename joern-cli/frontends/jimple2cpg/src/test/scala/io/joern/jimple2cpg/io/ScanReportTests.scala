package io.joern.jimple2cpg.io

import io.joern.jimple2cpg.{Config, Jimple2Cpg}
import io.joern.x2cpg.report.ScanReport
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Files
import javax.tools.ToolProvider

class ScanReportTests extends AnyWordSpec with Matchers {

  "jimple2cpg" should {

    "list the processed class files" in {
      FileUtil.usingTemporaryDirectory("jimple2cpgScanReport") { dir =>
        val source = dir / "Good.java"
        Files.writeString(source, "public class Good {\n  void f() {}\n}\n")
        val classes = Files.createDirectories(dir / "classes")
        val result  = ToolProvider.getSystemJavaCompiler.run(null, null, null, "-d", classes.toString, source.toString)
        result shouldBe 0

        val report = new ScanReport()
        new Jimple2Cpg().createCpg(Config().withInputPath(classes.toString), report).get.close()

        report.allProblems shouldBe empty
        report.fileEntries.map(entry => (entry.path, entry.parsed, entry.cpgGenerated)) shouldBe
          Seq(("Good.class", true, true))
      }
    }
  }
}
