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

    "export OpenWrtDerived real-firmware source-to-sink path evidence without fixture-id fallback" in {
      withDLinkStagingRows { stagingRows =>
        val sourceRows = stagingRows.flatMap(_("source_endpoints").arr.map(_.obj))
        val sinkRows   = stagingRows.flatMap(_("sink_endpoints").arr.map(_.obj))
        val pathRows   = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        sourceRows.exists(row =>
          row("module_path").str.endsWith("usr/lib/lua/luci/controller/mtkwifi.luac") &&
            row("callsite_id").str == "root.110@pc3" &&
            row("trigger").str == "luci.http.formvalue"
        ) shouldBe true

        sinkRows.exists(row =>
          row("module_path").str.endsWith("usr/lib/lua/luci/controller/mtkwifi.luac") &&
            row("callsite_id").str == "root.110@pc12" &&
            row("trigger").str == "os.execute"
        ) shouldBe true

        pathRows.size should be > 0
        pathRows.exists(row =>
          row("source_module_path").str.endsWith("usr/lib/lua/luci/controller/mtkwifi.luac") &&
            row("sink_module_path").str.endsWith("usr/lib/lua/luci/controller/mtkwifi.luac") &&
            row("source_pc").num.toInt == 3 &&
            row("sink_pc").num.toInt == 12 &&
            row("path_steps").arr.exists(_.str.contains("::")) &&
            row("path_steps").arr.exists(_.str.endsWith("usr/lib/lua/luci/controller/mtkwifi.luac::root.110@pc3:r0")) &&
            row("path_steps").arr.exists(_.str.endsWith("usr/lib/lua/luci/controller/mtkwifi.luac::root.110@pc12:r1"))
        ) shouldBe true

        pathRows.foreach { row =>
          row("source_module_path").str should not be empty
          row("sink_module_path").str should not be empty
          row("path_steps").arr.foreach { step =>
            step.str should include("::")
          }
        }
        pathRows.exists(row => row.obj.contains("callsite_id")) shouldBe false
      }
    }

    "export OpenWrtDerived root.61 fan-out source-to-sink path evidence" in {
      withDLinkStagingRows { stagingRows =>
        val sourceRows = stagingRows.flatMap(_("source_endpoints").arr.map(_.obj))
        val sinkRows   = stagingRows.flatMap(_("sink_endpoints").arr.map(_.obj))
        val pathRows   = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        sourceRows.exists(row =>
          row("module_path").str.endsWith("usr/lib/lua/luci/controller/mtkwifi.luac") &&
            row("callsite_id").str == "root.61@pc63" &&
            row("trigger").str == "luci.http.formvalue"
        ) shouldBe true

        val sinkCallsites = Vector(
          "root.61@pc180",
          "root.61@pc188",
          "root.61@pc196",
          "root.61@pc204",
          "root.61@pc212",
          "root.61@pc220",
          "root.61@pc228"
        )
        sinkCallsites.foreach { callsiteId =>
          sourceRows.exists(row =>
            row("module_path").str.endsWith("usr/lib/lua/luci/controller/mtkwifi.luac") &&
              row("callsite_id").str == "root.61@pc63" &&
              row("trigger").str == "luci.http.formvalue"
          ) shouldBe true

          sinkRows.exists(row =>
            row("module_path").str.endsWith("usr/lib/lua/luci/controller/mtkwifi.luac") &&
              row("callsite_id").str == callsiteId &&
              row("trigger").str == "os.execute"
          ) shouldBe true
        }

        sinkCallsites.foreach { callsiteId =>
          val sinkPc = callsiteId.split("@pc", 2)(1).toInt
          pathRows.exists(row =>
            row("source_module_path").str.endsWith("usr/lib/lua/luci/controller/mtkwifi.luac") &&
              row("sink_module_path").str.endsWith("usr/lib/lua/luci/controller/mtkwifi.luac") &&
              row("source_pc").num.toInt == 63 &&
              row("sink_pc").num.toInt == sinkPc &&
              row("source_trigger").str == "luci.http.formvalue" &&
              row("sink_trigger").str == "os.execute" &&
              row("path_steps").arr.forall(_.str.contains("::"))
          ) shouldBe true
        }
        pathRows.exists(row => row.obj.contains("callsite_id")) shouldBe false
      }
    }

    "export OpenWrtDerived cross-module webcmd to mtkwifi popen path evidence" in {
      withDLinkStagingRows { stagingRows =>
        val sourceRows = stagingRows.flatMap(_("source_endpoints").arr.map(_.obj))
        val sinkRows   = stagingRows.flatMap(_("sink_endpoints").arr.map(_.obj))
        val pathRows   = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        sourceRows.exists(row =>
          row("module_path").str.endsWith("usr/lib/lua/luci/controller/mtkwifi.luac") &&
            row("callsite_id").str == "root.2@pc4" &&
            row("trigger").str == "luci.http.formvalue"
        ) shouldBe true

        sinkRows.exists(row =>
          row("module_path").str.endsWith("usr/lib/lua/mtkwifi.luac") &&
            row("callsite_id").str == "root.12@pc5" &&
            row("trigger").str == "io.popen"
        ) shouldBe true

        pathRows.exists(row =>
          row("source_module_path").str.endsWith("usr/lib/lua/luci/controller/mtkwifi.luac") &&
            row("sink_module_path").str.endsWith("usr/lib/lua/mtkwifi.luac") &&
            row("source_pc").num.toInt == 4 &&
            row("sink_pc").num.toInt == 5 &&
            row("source_trigger").str == "luci.http.formvalue" &&
            row("sink_trigger").str == "io.popen" &&
            row("path_steps").arr.forall(_.str.contains("::"))
        ) shouldBe true
        pathRows.exists(row => row.obj.contains("callsite_id")) shouldBe false
      }
    }

    "export CrossPlatform real-firmware source and sink endpoints before path repair" in {
      withXiaomiStagingRows { stagingRows =>
        val sourceRows = stagingRows.flatMap(_("source_endpoints").arr.map(_.obj))
        val sinkRows   = stagingRows.flatMap(_("sink_endpoints").arr.map(_.obj))
        val callRows   = stagingRows.flatMap(_("call_name_resolution").arr.map(_.obj))
        val pathRows   = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        sourceRows.exists(row =>
          row("module_path").str.endsWith("usr/lib/lua/luci/controller/api/misystem.luac") &&
            row("callsite_id").str == "root.39@pc15" &&
            row("trigger").str == "luci.http.formvalue"
        ) shouldBe true

        sinkRows.exists(row =>
          row("module_path").str.endsWith("usr/lib/lua/xiaoqiang/util/XQQoSUtil.luac") &&
            row("callsite_id").str == "root.24@pc82" &&
            row("trigger").str == "os.execute"
        ) shouldBe true

        sinkRows.exists(row =>
          row("module_path").str.endsWith("usr/lib/lua/xiaoqiang/util/XQSynchrodata.luac") &&
            row("callsite_id").str == "root.0@pc25" &&
            row("trigger").str == "os.execute"
        ) shouldBe true

        sinkRows.exists(row =>
          row("module_path").str.endsWith("usr/lib/lua/luci/util.luac") &&
            row("callsite_id").str == "root@pc3" &&
            row("trigger").str == "io.popen"
        ) shouldBe true

        callRows.exists(row =>
          row("module_path").str.endsWith("usr/lib/lua/luci/controller/api/misystem.luac") &&
            row("callsite_id").str == "root.145@pc48" &&
            row("resolved_name").str == "luci.util.exec"
        ) shouldBe true

        pathRows.exists(row => row.obj.contains("callsite_id")) shouldBe false
      }
    }

    "export CrossPlatform representative source-to-sink path evidence" in {
      withXiaomiStagingRows { stagingRows =>
        val pathRows = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        pathRows.exists(row =>
          row("source_module_path").str.endsWith("usr/lib/lua/luci/controller/api/misystem.luac") &&
            row("sink_module_path").str.endsWith("usr/lib/lua/xiaoqiang/util/XQQoSUtil.luac") &&
            row("source_pc").num.toInt == 15 &&
            row("sink_pc").num.toInt == 82 &&
            row("source_trigger").str == "luci.http.formvalue" &&
            row("sink_trigger").str == "os.execute" &&
            row("path_steps").arr.forall(_.str.contains("::"))
        ) shouldBe true

        pathRows.exists(row =>
          row("source_module_path").str.endsWith("usr/lib/lua/luci/controller/api/misystem.luac") &&
            row("sink_module_path").str.endsWith("usr/lib/lua/xiaoqiang/util/XQSynchrodata.luac") &&
            row("source_pc").num.toInt == 15 &&
            row("sink_pc").num.toInt == 25 &&
            row("source_trigger").str == "luci.http.formvalue" &&
            row("sink_trigger").str == "os.execute" &&
            row("path_steps").arr.forall(_.str.contains("::"))
        ) shouldBe true

        pathRows.exists(row =>
          row("source_module_path").str.endsWith("usr/lib/lua/luci/controller/api/misystem.luac") &&
            row("sink_module_path").str.endsWith("usr/lib/lua/luci/util.luac") &&
            Set(15, 19).contains(row("source_pc").num.toInt) &&
            row("sink_pc").num.toInt == 3 &&
            row("source_trigger").str == "luci.http.formvalue" &&
            row("sink_trigger").str == "io.popen" &&
            row("path_steps").arr.forall(_.str.contains("::"))
        ) shouldBe true

        pathRows.exists(row =>
          row("source_module_path").str.endsWith("usr/lib/lua/luci/controller/api/misystem.luac") &&
            row("sink_module_path").str.endsWith("usr/lib/lua/luci/controller/api/misystem.luac") &&
            row("source_pc").num.toInt == 15 &&
            row("sink_pc").num.toInt == 38 &&
            row("source_trigger").str == "luci.http.formvalue" &&
            row("sink_trigger").str == "luci.util.exec" &&
            row("path_steps").arr.forall(_.str.contains("::"))
        ) shouldBe true
      }
    }
  }

  private def withDLinkStagingRows(test: Vector[ujson.Obj] => Unit): Unit = {
    val resourceRoot = Paths.get(getClass.getClassLoader.getResource("OpenWrtDerived-real-firmware-path-report").toURI)

    FileUtil.usingTemporaryDirectory("lua2cpg-OpenWrtDerived-real-firmware-path-report") { tmpDir =>
      val outputPath = tmpDir.resolve("OpenWrtDerived-real-firmware-path-report.cpg.bin").toString
      val exportDir  = tmpDir.resolve("real-firmware-export")
      val cpg = new Lua2Cpg()
        .createCpg(
          Config(realFirmwareOutputDir = Some(exportDir.toString))
            .withInputPath(resourceRoot.toString)
            .withOutputPath(outputPath)
        )
        .get
      cpg.close()

      val stagingDir    = exportDir.resolve("staging")
      val stagingStream = Files.list(stagingDir)
      val stagingFiles  = stagingStream.iterator.asScala.toVector
      try {
        stagingFiles.size should be > 0
        test(stagingFiles.map(path => ujson.read(Files.readString(path)).obj))
      } finally {
        stagingStream.close()
      }
    }
  }

  private def withXiaomiStagingRows(test: Vector[ujson.Obj] => Unit): Unit = {
    val resourceRoot = Paths.get(getClass.getClassLoader.getResource("CrossPlatform-real-firmware-path-report").toURI)

    FileUtil.usingTemporaryDirectory("lua2cpg-CrossPlatform-real-firmware-path-report") { tmpDir =>
      val outputPath = tmpDir.resolve("CrossPlatform-real-firmware-path-report.cpg.bin").toString
      val exportDir  = tmpDir.resolve("real-firmware-export")
      val cpg = new Lua2Cpg()
        .createCpg(
          Config(realFirmwareOutputDir = Some(exportDir.toString))
            .withInputPath(resourceRoot.toString)
            .withOutputPath(outputPath)
        )
        .get
      cpg.close()

      val stagingDir    = exportDir.resolve("staging")
      val stagingStream = Files.list(stagingDir)
      val stagingFiles  = stagingStream.iterator.asScala.toVector
      try {
        stagingFiles.size should be > 0
        test(stagingFiles.map(path => ujson.read(Files.readString(path)).obj))
      } finally {
        stagingStream.close()
      }
    }
  }
}
