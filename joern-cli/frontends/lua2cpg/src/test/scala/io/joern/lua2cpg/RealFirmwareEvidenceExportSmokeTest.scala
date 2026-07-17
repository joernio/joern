package io.joern.lua2cpg

import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

class RealFirmwareEvidenceExportSmokeTest extends AnyWordSpec with Matchers {

  "Lua2Cpg" should {
    "export Lua evidence with scoped callsite rows" in {
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

        val staging = stagingRows(exportDir)
          .find(_("relative_path").str.endsWith("d24-sanitizer-suppresses-report/input.luac"))
          .getOrElse(fail("missing sanitizer fixture staging evidence"))

        val callRows = staging("call_name_resolution").arr.map(_.obj)
        callRows.exists(row =>
          row("module_path").str.endsWith("d24-sanitizer-suppresses-report/input.luac") &&
            hasScopedCallsite(row, "root@pc20") &&
            row("resolved_name").str == "tonumber"
        ) shouldBe true

        val pathRows = staging("path_evidence").arr.map(_.obj)
        pathRows.exists(row =>
          row("path_steps").arr.exists(_.str.endsWith("d24-sanitizer-suppresses-report/input.luac::root@pc20:r2"))
        ) shouldBe true
      }
    }

    "export OpenWrt-derived source and sink endpoints" in {
      withOpenWrtDerivedExportDir { exportDir =>
        val rows       = stagingRows(exportDir)
        val sourceRows = rows.flatMap(_("source_endpoints").arr.map(_.obj))
        val sinkRows   = rows.flatMap(_("sink_endpoints").arr.map(_.obj))
        val pathRows   = rows.flatMap(_("path_evidence").arr.map(_.obj))

        sourceRows.exists(row =>
          row("module_path").str.endsWith("luci/controller/mtkwifi.luac") &&
            hasScopedCallsite(row, "root.110@pc3") &&
            row("trigger").str == "luci.http.formvalue"
        ) shouldBe true

        sinkRows.exists(row =>
          row("module_path").str.endsWith("luci/controller/mtkwifi.luac") &&
            hasScopedCallsite(row, "root.110@pc12") &&
            row("trigger").str == "os.execute"
        ) shouldBe true

        sinkRows.exists(row =>
          row("module_path").str.endsWith("luci/controller/hwnat.luac") &&
            row("trigger").str == "io.popen"
        ) shouldBe true

        pathRows.exists(row =>
          row("source_module_path").str.endsWith("luci/controller/mtkwifi.luac") &&
            row("sink_module_path").str.endsWith("luci/controller/mtkwifi.luac") &&
            row("source_pc").num.toInt == 3 &&
            row("sink_pc").num.toInt == 12 &&
            row("path_steps").arr.exists(_.str.endsWith("luci/controller/mtkwifi.luac::root.110@pc3:r0")) &&
            row("path_steps").arr.exists(_.str.endsWith("luci/controller/mtkwifi.luac::root.110@pc12:r1"))
        ) shouldBe true
      }
    }

    "export OpenWrt-derived path report totals with scoped path steps" in {
      withOpenWrtDerivedExportDir { exportDir =>
        val profile = ujson.read(Files.readString(exportDir.resolve("path-search-profile.json"))).obj
        profile("taint_path_count").num.toInt shouldBe 18
        profile("report_count").num.toInt shouldBe 18

        val pathRows = stagingRows(exportDir).flatMap(_("path_evidence").arr.map(_.obj))
        pathRows.size shouldBe 18
        pathRows.foreach { row =>
          row("source_module_path").str should not be empty
          row("sink_module_path").str should not be empty
          row("path_steps").arr should not be empty
          row("path_steps").arr.foreach { step =>
            step.str should include("::")
          }
        }
        pathRows.exists(row => row.obj.contains("callsite_id")) shouldBe false
      }
    }
  }

  private def withOpenWrtDerivedExportDir(test: Path => Unit): Unit = {
    val resourceRoot = Paths.get(getClass.getClassLoader.getResource("openwrt-derived-firmware-lua/usr/lib/lua").toURI)

    FileUtil.usingTemporaryDirectory("lua2cpg-openwrt-derived-real-firmware-export") { tmpDir =>
      val outputPath = tmpDir.resolve("openwrt-derived-firmware-lua.cpg.bin").toString
      val exportDir  = tmpDir.resolve("openwrt-derived-firmware-lua-evidence")
      val cpg = new Lua2Cpg()
        .createCpg(
          Config(realFirmwareOutputDir = Some(exportDir.toString))
            .withInputPath(resourceRoot.toString)
            .withOutputPath(outputPath)
        )
        .get
      cpg.close()

      test(exportDir)
    }
  }

  private def stagingRows(exportDir: Path) = {
    val stagingStream = Files.list(exportDir.resolve("staging"))
    try {
      val rows = stagingStream.iterator.asScala.toVector.map(path => ujson.read(Files.readString(path)).obj)
      rows should not be empty
      rows
    } finally {
      stagingStream.close()
    }
  }

  private def hasScopedCallsite(row: ujson.Obj, localCallsiteId: String): Boolean =
    row("callsite_id").str.contains("::") && row("callsite_id").str.endsWith(s"::$localCallsiteId")
}
