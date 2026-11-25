package io.joern.rust2cpg.io

import io.joern.rust2cpg.{Config, Rust2Cpg}
import io.joern.x2cpg.report.ScanReport
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}

class ScanReportTests extends AnyWordSpec with Matchers {

  "rust2cpg" should {

    // rust-analyzer recovers from syntax errors, so there is no reliable way to make it skip a file
    "not report files that can be parsed" in {
      FileUtil.usingTemporaryDirectory("rust2cpgScanReport") { dir =>
        Files.writeString(
          dir / "Cargo.toml",
          """[package]
            |name = "simple"
            |version = "0.1.0"
            |edition = "2021"
            |""".stripMargin
        )
        Files.createDirectories(dir / "src")
        Files.writeString(dir / "src" / "lib.rs", "pub fn good() -> i32 { 1 }\n")

        val report = new ScanReport()
        new Rust2Cpg().createCpg(Config().withInputPath(dir.toString).withNoSysRoot(true), report).get.close()

        report.allProblems shouldBe empty
        report.fileEntries.map(entry => (entry.path, entry.loc, entry.parsed, entry.cpgGenerated)) shouldBe
          Seq((Paths.get("src", "lib.rs").toString, Some(1), true, true))
      }
    }
  }
}
