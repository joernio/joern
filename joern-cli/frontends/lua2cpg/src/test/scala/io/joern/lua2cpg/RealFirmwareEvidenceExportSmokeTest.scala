package io.joern.lua2cpg

import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*

class RealFirmwareEvidenceExportSmokeTest extends AnyWordSpec with Matchers {

  "Lua2Cpg" should {
    "export Lua real-firmware evidence with scoped callsite rows" in {
      val resourceRoot = Paths.get(getClass.getClassLoader.getResource("rules-sanitizer-report").toURI)

      FileUtil.usingTemporaryDirectory("lua2cpg-real-firmware-export-smoke") { tmpDir =>
        val outputPath = tmpDir.resolve("rules-sanitizer-report.cpg.bin").toString
        val exportDir  = tmpDir.resolve("real-firmware-export")
        val cpg = new Lua2Cpg()
          .createCpg(
            Config(realFirmwareOutputDir = Some(exportDir.toString))
              .withInputPath(resourceRoot.toString)
              .withOutputPath(outputPath)
          )
          .get
        cpg.close()

        Files.isRegularFile(exportDir.resolve("decoder-summary.json")) shouldBe true
        Files.isRegularFile(exportDir.resolve("run-summary.json")) shouldBe true
        Files.isRegularFile(exportDir.resolve("run-errors.json")) shouldBe true
        Files.isRegularFile(exportDir.resolve("path-search-profile.json")) shouldBe true

        val stagingDir    = exportDir.resolve("staging")
        val stagingStream = Files.list(stagingDir)
        val stagingFiles  = stagingStream.iterator.asScala.toVector
        try {
          stagingFiles.size should be > 0

          val staging = stagingFiles
            .map(path => ujson.read(Files.readString(path)).obj)
            .find(_("relative_path").str.endsWith("d24-sanitizer-suppresses-report/input.luac"))
            .getOrElse(fail("missing sanitizer fixture staging evidence"))

          val callRows = staging("call_name_resolution").arr.map(_.obj)
          callRows.exists(row =>
            row("module_path").str.endsWith("d24-sanitizer-suppresses-report/input.luac") &&
              row("callsite_id").str == "root@pc20" &&
              row("resolved_name").str == "tonumber"
          ) shouldBe true

          val pathRows = staging("path_evidence").arr.map(_.obj)
          pathRows.exists(row =>
            row("path_steps").arr.exists(_.str.endsWith("d24-sanitizer-suppresses-report/input.luac::root@pc20:r2"))
          ) shouldBe true
        } finally {
          stagingStream.close()
        }
      }
    }
  }
}
