package io.joern.c2cpg.io

import io.joern.c2cpg.{C2Cpg, Config, Main}
import io.joern.x2cpg.report.{Category, ScanReport}
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Files

class ScanReportTests extends AnyWordSpec with Matchers {

  private def withProject[T](f: (java.nio.file.Path, java.nio.file.Path) => T): T = {
    FileUtil.usingTemporaryDirectory("c2cpgScanReport") { dir =>
      val src = dir / "src"
      Files.createDirectories(src)
      Files.writeString(src / "main.c", "int main() {\n  return 0;\n}\n")
      f(src, dir / "out")
    }
  }

  "c2cpg" should {

    "write the scan report next to the CPG with --report-json" in withProject { (src, out) =>
      Files.createDirectories(out)
      val cpg = out / "cpg.bin"
      Main.main(Array(src.toString, "--output", cpg.toString, "--report-json"))

      val json = ujson.read(Files.readString(out / "cpg.bin.report.json"))
      json("frontend").str shouldBe "C2Cpg"
      json("status").str shouldBe "Completed"
      json("cpgPath").str shouldBe cpg.toString
      json("problems").arr shouldBe empty
      val files = json("files").arr
      files.map(_("path").str).toSeq shouldBe Seq("main.c")
      files.head("loc").num shouldBe 3
      files.head("parsed").bool shouldBe true
      files.head("cpgGenerated").bool shouldBe true
    }

    "write the scan report to the given file with --report-json-file" in withProject { (src, out) =>
      Files.createDirectories(out)
      val report = out / "elsewhere" / "scan.json"
      Main.main(Array(src.toString, "--output", (out / "cpg.bin").toString, "--report-json-file", report.toString))

      ujson.read(Files.readString(report))("summary")("files").num shouldBe 1
      Files.exists(out / "cpg.bin.report.json") shouldBe false
    }

    "not write a scan report by default" in withProject { (src, out) =>
      Files.createDirectories(out)
      Main.main(Array(src.toString, "--output", (out / "cpg.bin").toString))

      Files.exists(out / "cpg.bin") shouldBe true
      Files.exists(out / "cpg.bin.report.json") shouldBe false
    }

    "report a compilation database that cannot be parsed as a problem" in withProject { (src, out) =>
      Files.createDirectories(out)
      val compilationDatabase = out / "compile_commands.json"
      Files.writeString(compilationDatabase, "{ this is not json")

      val report = new ScanReport()
      val config = Config().withInputPath(src.toString).withCompilationDatabase(compilationDatabase.toString)
      new C2Cpg().createCpg(config, report).get.close()

      val problem = report.allProblems.find(_.message.contains("Could not parse")).getOrElse(fail("no problem"))
      problem.category shouldBe Category.FileParse
      problem.file shouldBe Some(compilationDatabase.toString)
      problem.exception should not be empty
    }
  }
}
