package io.joern.lua2cpg

import io.joern.lua2cpg.bytecode.{
  LuaPairPerformanceProfile,
  LuaPathSearchStats,
  LuaPerformanceAttribution,
  LuaProgramSemantics,
  LuaRealFirmwareEvidenceExporter,
  LuaTaintPath
}
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
            hasScopedCallsite(row, "root@pc19") &&
            row("resolved_name").str == "tonumber"
        ) shouldBe true

        val pathRows = staging("path_evidence").arr.map(_.obj)
        pathRows.exists(row =>
          row("path_steps").arr.exists(_.str.endsWith("d24-sanitizer-suppresses-report/input.luac::root@pc19:r1"))
        ) shouldBe true
      }
    }

    "reject fixture paths without real endpoints before creating output" in {
      val counterNames = Vector(
        "source_reachability_check_count",
        "source_reachability_accepted_count",
        "prototype_unreachable_pair_count",
        "source_specific_provenance_pruned_pair_count",
        "parameter_position_check_count",
        "parameter_position_accepted_count",
        "parameter_position_pruned_count",
        "path_constructor_check_count",
        "path_constructor_accepted_count",
        "path_constructor_pruned_count",
        "bridge_argument_provenance_candidate_count",
        "bridge_candidate_pc_pruned_count",
        "bridge_candidate_reachability_pruned_count",
        "bridge_local_path_attempt_count",
        "bridge_local_path_success_count",
        "local_path_search_count",
        "distinct_local_path_query_count",
        "local_path_cache_hit_count",
        "local_path_cache_miss_count",
        "local_path_graph_build_count",
        "local_path_graph_cache_hit_count",
        "local_path_graph_cache_miss_count",
        "bridge_path_cache_hit_count",
        "bridge_path_cache_miss_count",
        "targeted_search_node_visit_count",
        "targeted_search_edge_visit_count",
        "early_candidate_short_circuit_count",
        "taint_path_count",
        "report_count"
      )
      val counters = counterNames.map(_ -> 0L).toMap ++ Map(
        "source_reachability_check_count"   -> 1L,
        "source_reachability_accepted_count" -> 1L,
        "parameter_position_check_count"    -> 1L,
        "parameter_position_accepted_count" -> 1L,
        "path_constructor_check_count"      -> 1L,
        "path_constructor_accepted_count"   -> 1L,
        "local_path_search_count"           -> 1L,
        "distinct_local_path_query_count"   -> 1L,
        "local_path_cache_miss_count"       -> 1L,
        "taint_path_count"                  -> 1L
      )
      val sourceRef = "bc-endpoint-contract/input.luac:root@pc1:r0"
      val sinkRef   = "bc-endpoint-contract/input.luac:root@pc2:r1"
      val pair = LuaPairPerformanceProfile(
        sourceRef,
        sinkRef,
        "bc-endpoint-contract/input.luac::root@pc1",
        "bc-endpoint-contract/input.luac::root@pc2",
        "luci.http.formvalue",
        "os.execute",
        counters
      )
      val semantics = LuaProgramSemantics(
        moduleResolutions = Vector.empty,
        moduleReturnTables = Vector.empty,
        moduleFieldCallTargets = Vector.empty,
        interproceduralArgFlows = Vector.empty,
        interproceduralReturnFlows = Vector.empty,
        crossBoundaryCallTargets = Vector.empty,
        taintPaths = Vector(LuaTaintPath(sourceRef, sinkRef, Vector(sourceRef, sinkRef), "true-positive", "bytecode-only")),
        boundaries = Vector.empty,
        ruleMatches = Vector.empty,
        sourceEndpoints = Vector.empty,
        sinkEndpoints = Vector.empty,
        sanitizerCalls = Vector.empty,
        sanitizerClassifications = Vector.empty,
        reportClassifications = Vector.empty,
        vulnerabilityReports = Vector.empty,
        e5Boundaries = Vector.empty,
        pathSearchStats = LuaPathSearchStats(0, 0, 1, 1, 1, 1, 0),
        performanceAttribution = LuaPerformanceAttribution(1, 0, 1, 0, Vector(pair), counters)
      )

      FileUtil.usingTemporaryDirectory("lua2cpg-endpoint-contract") { tmpDir =>
        val exportDir = tmpDir.resolve("must-not-exist")
        val error = intercept[IllegalStateException] {
          LuaRealFirmwareEvidenceExporter.write(
            Config(realFirmwareOutputDir = Some(exportDir.toString)),
            Vector.empty,
            semantics
          )
        }
        error.getMessage should include("taint path lacks source endpoint")
        Files.exists(exportDir) shouldBe false
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
        profile("taint_path_count").num.toInt shouldBe 20
        profile("report_count").num.toInt shouldBe 20

        val pathRows = stagingRows(exportDir).flatMap(_("path_evidence").arr.map(_.obj))
        pathRows.size shouldBe 20
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
