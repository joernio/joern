package io.joern.lua2cpg

import io.joern.lua2cpg.bytecode.*
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.charset.StandardCharsets
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
              hasScopedCallsite(row, "root@pc20") &&
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
            hasScopedCallsite(row, "root.110@pc3") &&
            row("trigger").str == "luci.http.formvalue"
        ) shouldBe true

        sinkRows.exists(row =>
          row("module_path").str.endsWith("usr/lib/lua/luci/controller/mtkwifi.luac") &&
            hasScopedCallsite(row, "root.110@pc12") &&
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
            hasScopedCallsite(row, "root.61@pc63") &&
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
              hasScopedCallsite(row, "root.61@pc63") &&
              row("trigger").str == "luci.http.formvalue"
          ) shouldBe true

          sinkRows.exists(row =>
            row("module_path").str.endsWith("usr/lib/lua/luci/controller/mtkwifi.luac") &&
              hasScopedCallsite(row, callsiteId) &&
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
        val linkRows   = stagingRows.flatMap(_("module_linkage").arr.map(_.obj))
        val pathRows   = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        sourceRows.exists(row =>
          row("module_path").str.endsWith("usr/lib/lua/luci/controller/mtkwifi.luac") &&
            hasScopedCallsite(row, "root.55@pc3") &&
            row("trigger").str == "luci.http.formvalue"
        ) shouldBe true

        linkRows.exists(row =>
          row("module_path").str.endsWith("usr/lib/lua/luci/controller/mtkwifi.luac") &&
            hasScopedCallsite(row, "root.55@pc13") &&
            row("target_module_path").str.endsWith("usr/lib/lua/mtkwifi.luac") &&
            row("target_prototype_id").str == "root.12" &&
            row("field_name").str == "read_pipe"
        ) shouldBe true

        sinkRows.exists(row =>
          row("module_path").str.endsWith("usr/lib/lua/mtkwifi.luac") &&
            hasScopedCallsite(row, "root.12@pc5") &&
            row("trigger").str == "io.popen"
        ) shouldBe true

        pathRows.exists(row =>
          row("source_module_path").str.endsWith("usr/lib/lua/luci/controller/mtkwifi.luac") &&
            row("sink_module_path").str.endsWith("usr/lib/lua/mtkwifi.luac") &&
            row("source_pc").num.toInt == 3 &&
            row("sink_pc").num.toInt == 5 &&
            row("source_trigger").str == "luci.http.formvalue" &&
            row("sink_trigger").str == "io.popen" &&
            row("path_steps").arr.forall(_.str.contains("::"))
        ) shouldBe true
        pathRows.exists(row =>
          row("source_module_path").str.endsWith("usr/lib/lua/luci/controller/mtkwifi.luac") &&
            row("sink_module_path").str.endsWith("usr/lib/lua/mtkwifi.luac") &&
            row("source_pc").num.toInt == 4 &&
            row("source_function_name").str == "root.2" &&
            row("sink_pc").num.toInt == 5 &&
            row("sink_function_name").str == "root.12"
        ) shouldBe false
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
            hasScopedCallsite(row, "root.39@pc15") &&
            row("trigger").str == "luci.http.formvalue"
        ) shouldBe true

        sinkRows.exists(row =>
          row("module_path").str.endsWith("usr/lib/lua/xiaoqiang/util/XQQoSUtil.luac") &&
            hasScopedCallsite(row, "root.24@pc82") &&
            row("trigger").str == "os.execute"
        ) shouldBe true

        sinkRows.exists(row =>
          row("module_path").str.endsWith("usr/lib/lua/xiaoqiang/util/XQSynchrodata.luac") &&
            hasScopedCallsite(row, "root.0@pc25") &&
            row("trigger").str == "os.execute"
        ) shouldBe true

        sinkRows.exists(row =>
          row("module_path").str.endsWith("usr/lib/lua/luci/util.luac") &&
            hasScopedCallsite(row, "root.36@pc3") &&
            row("trigger").str == "io.popen"
        ) shouldBe true

        callRows.exists(row =>
          row("module_path").str.endsWith("usr/lib/lua/luci/controller/api/misystem.luac") &&
            hasScopedCallsite(row, "root.145@pc48") &&
            row("resolved_name").str == "luci.util.exec"
        ) shouldBe true

        sinkRows.exists(row =>
          row("module_path").str.endsWith("usr/lib/lua/luci/controller/api/misystem.luac") &&
            hasScopedCallsite(row, "root.94@pc38") &&
            row("trigger").str == "luci.util.exec"
        ) shouldBe true

        pathRows.exists(row => row.obj.contains("callsite_id")) shouldBe false
      }
    }

    "export CrossPlatform representative source-to-sink path evidence" in {
      withXiaomiStagingRows { stagingRows =>
        val pathRows = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))
        val misystem = "usr/lib/lua/luci/controller/api/misystem.luac"

        pathRows.exists(row =>
          row("source_module_path").str.endsWith(misystem) &&
            row("sink_module_path").str.endsWith("usr/lib/lua/xiaoqiang/util/XQQoSUtil.luac") &&
            row("source_pc").num.toInt == 15 &&
            row("sink_pc").num.toInt == 82 &&
            row("source_trigger").str == "luci.http.formvalue" &&
            row("sink_trigger").str == "os.execute" &&
            row("path_steps").arr.forall(_.str.contains("::")) &&
            row("path_steps").arr.exists(_.str == s"$misystem::root.63@pc23:r7") &&
            row("path_steps").arr.exists(_.str == s"$misystem::root.63@pc23:r6")
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

    "distinguish source-value bridge proof from representative bridge proof" in {
      withXiaomiStagingRows { stagingRows =>
        val sourceRows = stagingRows.flatMap(_("source_endpoints").arr.map(_.obj))
        val sinkRows   = stagingRows.flatMap(_("sink_endpoints").arr.map(_.obj))
        val bridgeRows = stagingRows.flatMap(_("interproc_arg_flow").arr.map(_.obj))
        val defuseRows = stagingRows.flatMap(_("defuse_paths").arr.map(_.obj))
        val pathRows   = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        def hasSource(moduleSuffix: String, pc: Int, trigger: String): Boolean =
          sourceRows.exists(row =>
            row("module_path").str.endsWith(moduleSuffix) &&
              row("callsite_id").str.endsWith(s"@pc$pc") &&
              row("callsite_id").str.contains("::") &&
              row("trigger").str == trigger
          )

        def hasSink(moduleSuffix: String, pc: Int, trigger: String): Boolean =
          sinkRows.exists(row =>
            row("module_path").str.endsWith(moduleSuffix) &&
              row("callsite_id").str.endsWith(s"@pc$pc") &&
              row("callsite_id").str.contains("::") &&
              row("trigger").str == trigger
          )

        def hasBridge(
          sourceModuleSuffix: String,
          sourceCallsiteSuffix: String,
          targetModuleSuffix: String,
          targetPrototypeId: String,
          argumentIndex: Int
        ): Boolean =
          bridgeRows.exists(row =>
            row("callsite_id").str.contains("::") &&
              row("callsite_id").str.endsWith(sourceCallsiteSuffix) &&
              row("from_argument_ref").str.contains(sourceModuleSuffix) &&
              row("argument_index").num.toInt == argumentIndex &&
              row("target_module_path").str.endsWith(targetModuleSuffix) &&
              row("target_prototype_id").str == targetPrototypeId
          )

        def pathByPc(
          sourceModuleSuffix: String,
          sourcePc: Int,
          sourceTrigger: String,
          sinkModuleSuffix: String,
          sinkPc: Int,
          sinkTrigger: String
        ) =
          pathRows.find(row =>
            row("source_module_path").str.endsWith(sourceModuleSuffix) &&
              row("source_pc").num.toInt == sourcePc &&
              row("source_trigger").str == sourceTrigger &&
              row("sink_module_path").str.endsWith(sinkModuleSuffix) &&
              row("sink_pc").num.toInt == sinkPc &&
              row("sink_trigger").str == sinkTrigger &&
              row("path_steps").arr.nonEmpty &&
              row("path_steps").arr.forall(_.str.contains("::"))
          )

        def pathByFunction(
          sourceModuleSuffix: String,
          sourceFunctionName: String,
          sourcePc: Int,
          sourceTrigger: String,
          sinkModuleSuffix: String,
          sinkFunctionName: String,
          sinkPc: Int,
          sinkTrigger: String
        ) =
          pathByPc(sourceModuleSuffix, sourcePc, sourceTrigger, sinkModuleSuffix, sinkPc, sinkTrigger)
            .filter(row =>
              row("source_function_name").str == sourceFunctionName &&
                row("sink_function_name").str == sinkFunctionName
            )

        hasSource("usr/lib/lua/luci/controller/api/misystem.luac", 57, "luci.http.formvalue") shouldBe true
        hasSink("usr/lib/lua/xiaoqiang/common/XQFunction.luac", 35, "os.execute") shouldBe true
        hasBridge(
          "usr/lib/lua/luci/controller/api/misystem.luac",
          "root.37@pc114",
          "usr/lib/lua/xiaoqiang/common/XQFunction.luac",
          "root.33",
          1
        ) shouldBe true

        pathByPc(
          "usr/lib/lua/luci/controller/api/misystem.luac",
          57,
          "luci.http.formvalue",
          "usr/lib/lua/xiaoqiang/common/XQFunction.luac",
          35,
          "os.execute"
        ).isDefined shouldBe true

        defuseRows.exists(row =>
          row("source_ref").str == "root.37@pc57:r15" &&
            row("sink_ref").str == "root.37@pc114:r28" &&
            row("first_missing_edge").str == "none"
        ) shouldBe false

        val strictBridgePath = pathByFunction(
          "usr/lib/lua/luci/controller/api/misystem.luac",
          "memTestConfig",
          14,
          "luci.http.formvalue",
          "usr/lib/lua/xiaoqiang/common/XQFunction.luac",
          "nvramSet",
          35,
          "os.execute"
        )
        strictBridgePath.isDefined shouldBe true
        val strictBridgeSteps = strictBridgePath.get("path_steps").arr.map(_.str).toSet
        strictBridgeSteps should contain("usr/lib/lua/luci/controller/api/misystem.luac::root.151@pc22:r6")
        strictBridgeSteps should contain("usr/lib/lua/xiaoqiang/common/XQFunction.luac::root.33:r1")
        strictBridgeSteps should contain("usr/lib/lua/xiaoqiang/common/XQFunction.luac::root.33@pc30:r3")
      }
    }

    "preserve CrossPlatform r5 representative bridge rows without unscoped fallback" in {
      withXiaomiStagingRows { stagingRows =>
        val sourceRows = stagingRows.flatMap(_("source_endpoints").arr.map(_.obj))
        val sinkRows   = stagingRows.flatMap(_("sink_endpoints").arr.map(_.obj))
        val pathRows   = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        val sourceModule = "usr/lib/lua/luci/controller/api/xqsystem.luac"
        val sinkModule   = "usr/lib/lua/xiaoqiang/common/XQFunction.luac"

        sourceRows.exists(row =>
          row("module_path").str.endsWith(sourceModule) &&
            row("callsite_id").str.contains("::") &&
            row("callsite_id").str.endsWith("::root.40@pc31") &&
            row("trigger").str == "luci.http.formvalue"
        ) shouldBe true

        sinkRows.exists(row =>
          row("module_path").str.endsWith(sinkModule) &&
            row("callsite_id").str.endsWith("::root.33@pc35") &&
            row("trigger").str == "os.execute"
        ) shouldBe true

        val representativePath = pathRows.find(row =>
          row("source_module_path").str.endsWith(sourceModule) &&
            row("source_function_name").str == "setRouter" &&
            row("source_pc").num.toInt == 31 &&
            row("source_trigger").str == "luci.http.formvalue" &&
            row("sink_module_path").str.endsWith(sinkModule) &&
            row("sink_function_name").str == "nvramSet" &&
            row("sink_pc").num.toInt == 35 &&
            row("sink_trigger").str == "os.execute" &&
            row("path_steps").arr.nonEmpty &&
            row("path_steps").arr.forall(_.str.contains("::"))
        )

        representativePath.isDefined shouldBe true
        representativePath.get.obj.contains("callsite_id") shouldBe false

        val steps = representativePath.get("path_steps").arr.map(_.str)
        steps.exists(_.startsWith(s"$sourceModule::")) shouldBe true
        steps.exists(_.startsWith(s"$sinkModule::")) shouldBe true
      }
    }

    "export CrossPlatform r5 residual sink endpoints and source-to-sink paths" in {
      withXiaomiStagingRows { stagingRows =>
        val sinkRows = stagingRows.flatMap(_("sink_endpoints").arr.map(_.obj))
        val pathRows = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        def hasSink(moduleSuffix: String, pc: Int, trigger: String): Boolean =
          sinkRows.exists(row =>
            row("module_path").str.endsWith(moduleSuffix) &&
              row("callsite_id").str.endsWith(s"@pc$pc") &&
              row("callsite_id").str.contains("::") &&
              row("trigger").str == trigger
          )

        def hasPath(
          sourceModuleSuffix: String,
          sourceFunctionName: String,
          sourcePc: Int,
          sinkModuleSuffix: String,
          sinkFunctionName: String,
          sinkPc: Int,
          sinkTrigger: String
        ): Boolean =
          pathRows.exists(row =>
            row("source_module_path").str.endsWith(sourceModuleSuffix) &&
              row("source_function_name").str == sourceFunctionName &&
              row("source_pc").num.toInt == sourcePc &&
              row("sink_module_path").str.endsWith(sinkModuleSuffix) &&
              row("sink_function_name").str == sinkFunctionName &&
              row("sink_pc").num.toInt == sinkPc &&
              row("sink_trigger").str == sinkTrigger &&
              row("path_steps").arr.nonEmpty &&
              row("path_steps").arr.forall(_.str.contains("::"))
          )

        hasSink(
          "usr/lib/lua/luci/controller/api/xqnetwork.luac",
          276,
          "test.api.Process.forkExec"
        ) shouldBe true
        hasSink(
          "usr/lib/lua/xiaoqiang/module/XQEcos.luac",
          15,
          "test.api.Process.forkExec"
        ) shouldBe true
        hasSink(
          "usr/lib/lua/xiaoqiang/util/XQSysUtil.luac",
          112,
          "test.api.Process.forkExec"
        ) shouldBe true
        hasSink("usr/lib/lua/xiaoqiang/common/XQFunction.luac", 56, "forkExec") shouldBe true

        hasPath(
          "usr/lib/lua/luci/controller/api/misystem.luac",
          "networkAccessControlStatus",
          8,
          "usr/lib/lua/xiaoqiang/module/XQParentControl.luac",
          "get_macfilter_wan",
          10,
          "luci.util.exec"
        ) shouldBe true
        hasPath(
          "usr/lib/lua/luci/controller/api/misystem.luac",
          "parentalctlSetUrl",
          8,
          "usr/lib/lua/xiaoqiang/util/XQSynchrodata.luac",
          "root.0",
          25,
          "os.execute"
        ) shouldBe true
        hasPath(
          "usr/lib/lua/luci/controller/api/misystem.luac",
          "parentalctlSetUrl",
          8,
          "usr/lib/lua/xiaoqiang/common/XQFunction.luac",
          "thrift_tunnel_to_datacenter",
          22,
          "luci.util.exec"
        ) shouldBe true
        hasPath(
          "usr/lib/lua/luci/controller/api/misystem.luac",
          "qosApp",
          16,
          "usr/lib/lua/luci/controller/api/misystem.luac",
          "qosApp",
          102,
          "os.execute"
        ) shouldBe true
        hasPath(
          "usr/lib/lua/luci/controller/api/xqsystem.luac",
          "ExtendWifiConnectInitedRouter",
          32,
          "usr/lib/lua/xiaoqiang/util/XQWifiUtil.luac",
          "apcli_get_connect",
          24,
          "luci.util.exec"
        ) shouldBe true
        hasPath(
          "usr/lib/lua/luci/controller/api/xqsystem.luac",
          "ExtendWifiConnectInitedRouter",
          32,
          "usr/lib/lua/xiaoqiang/module/XQAPModule.luac",
          "extendwifi_set_connect",
          139,
          "luci.util.exec"
        ) shouldBe true
        hasPath(
          "usr/lib/lua/luci/controller/api/xqsystem.luac",
          "setPassword",
          16,
          "usr/lib/lua/xiaoqiang/util/XQSecureUtil.luac",
          "decCiphertext",
          46,
          "os.execute"
        ) shouldBe true
        hasPath(
          "usr/lib/lua/luci/controller/service/datacenter.luac",
          "setSyncRouterFile",
          6,
          "usr/lib/lua/luci/controller/service/datacenter.luac",
          "tunnelRequestDatacenter",
          24,
          "luci.util.exec"
        ) shouldBe true
      }
    }

    "export CrossPlatform r7 regressed source-to-sink paths without unscoped fallback" in {
      withXiaomiStagingRows { stagingRows =>
        val pathRows = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        def hasPath(
          sourceModuleSuffix: String,
          sourceFunctionName: String,
          sourcePc: Int,
          sinkModuleSuffix: String,
          sinkFunctionName: String,
          sinkPc: Int,
          sinkTrigger: String
        ): Boolean =
          pathRows.exists(row =>
            row("source_module_path").str.endsWith(sourceModuleSuffix) &&
              row("source_function_name").str == sourceFunctionName &&
              row("source_pc").num.toInt == sourcePc &&
              row("source_trigger").str == "luci.http.formvalue" &&
              row("sink_module_path").str.endsWith(sinkModuleSuffix) &&
              row("sink_function_name").str == sinkFunctionName &&
              row("sink_pc").num.toInt == sinkPc &&
              row("sink_trigger").str == sinkTrigger &&
              row("path_steps").arr.nonEmpty &&
              row("path_steps").arr.forall(_.str.contains("::")) &&
              !row.obj.contains("callsite_id")
          )

        val missingFamilies = Vector(
          "xqsmarthome.requestMitv@pc3->luci.util.exec@pc3" -> hasPath(
            "usr/lib/lua/luci/controller/api/xqsmarthome.luac",
            "requestMitv",
            3,
            "usr/lib/lua/luci/util.luac",
            "exec",
            3,
            "io.popen"
          ),
          "xqsystem.sysRecovery@pc12->XQFunction.nvramSet@pc35" -> hasPath(
            "usr/lib/lua/luci/controller/api/xqsystem.luac",
            "sysRecovery",
            12,
            "usr/lib/lua/xiaoqiang/common/XQFunction.luac",
            "nvramSet",
            35,
            "os.execute"
          ),
          "misystem.setLanApMode_Init@pc22->XQSynchrodata.func_unknow_0_0@pc25" -> hasPath(
            "usr/lib/lua/luci/controller/api/misystem.luac",
            "setLanApMode_Init",
            22,
            "usr/lib/lua/xiaoqiang/util/XQSynchrodata.luac",
            "func_unknow_0_0",
            25,
            "os.execute"
          ),
          "misystem.setWifiApMode@pc61->XQSynchrodata.func_unknow_0_0@pc25" -> hasPath(
            "usr/lib/lua/luci/controller/api/misystem.luac",
            "setWifiApMode",
            61,
            "usr/lib/lua/xiaoqiang/util/XQSynchrodata.luac",
            "func_unknow_0_0",
            25,
            "os.execute"
          ),
          "xqnetwork.pppoeStatus@pc6->luci.util.exec@pc3" -> hasPath(
            "usr/lib/lua/luci/controller/api/xqnetwork.luac",
            "pppoeStatus",
            6,
            "usr/lib/lua/luci/util.luac",
            "exec",
            3,
            "io.popen"
          ),
          "xqnetwork.setPeerWifiAutoAPMode@pc42->XQWifiUtil.apcli_set_inactive@pc75" -> hasPath(
            "usr/lib/lua/luci/controller/api/xqnetwork.luac",
            "setPeerWifiAutoAPMode",
            42,
            "usr/lib/lua/xiaoqiang/util/XQWifiUtil.luac",
            "apcli_set_inactive",
            75,
            "os.execute"
          ),
          "miats.getWifiMacfilterInfo@pc70->luci.util.exec@pc3" -> hasPath(
            "usr/lib/lua/luci/controller/api/miats.luac",
            "getWifiMacfilterInfo",
            70,
            "usr/lib/lua/luci/util.luac",
            "exec",
            3,
            "io.popen"
          )
        ).collect { case (label, false) => label }

        withClue(s"missing families: ${missingFamilies.mkString(", ")}") {
          missingFamilies shouldBe empty
        }
      }
    }

    "export CrossPlatform r7 miats pc148 sink endpoint with exact module scope" in {
      withXiaomiStagingRows { stagingRows =>
        val sinkRows = stagingRows.flatMap(_("sink_endpoints").arr.map(_.obj))

        sinkRows.count(row =>
          row("module_path").str.endsWith("usr/lib/lua/luci/controller/api/miats.luac") &&
            row("callsite_id").str.contains("::") &&
            row("callsite_id").str.endsWith("@pc148") &&
            row("trigger").str == "luci.util.exec"
        ) shouldBe 1
      }
    }

    "export CrossPlatform r7 setWifiApMode to nvramSet path with exact module scope" in {
      withXiaomiStagingRows { stagingRows =>
        val pathRows = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        pathRows.exists(row =>
          row("source_module_path").str.endsWith("usr/lib/lua/luci/controller/api/xqnetwork.luac") &&
            row("source_function_name").str == "setWifiApMode" &&
            row("source_pc").num.toInt == 28 &&
            row("source_trigger").str == "luci.http.formvalue" &&
            row("sink_module_path").str.endsWith("usr/lib/lua/xiaoqiang/common/XQFunction.luac") &&
            row("sink_function_name").str == "nvramSet" &&
            row("sink_pc").num.toInt == 35 &&
            row("sink_trigger").str == "os.execute" &&
            row("path_steps").arr.nonEmpty &&
            row("path_steps").arr.forall(_.str.contains("::")) &&
            !row.obj.contains("callsite_id")
        ) shouldBe true
      }
    }

    "export CrossPlatform r7 setWifiApMode to XQSynchrodata path with exact module scope" in {
      withXiaomiStagingRows { stagingRows =>
        val pathRows = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))
        val targetPath = pathRows.find(row =>
          row("source_module_path").str.endsWith("usr/lib/lua/luci/controller/api/xqnetwork.luac") &&
            row("source_function_name").str == "setWifiApMode" &&
            row("source_pc").num.toInt == 28 &&
            row("source_trigger").str == "luci.http.formvalue" &&
            row("sink_module_path").str.endsWith("usr/lib/lua/xiaoqiang/util/XQSynchrodata.luac") &&
            row("sink_function_name").str == "func_unknow_0_0" &&
            row("sink_pc").num.toInt == 25 &&
            row("sink_trigger").str == "os.execute"
        )

        targetPath.isDefined shouldBe true
        val pathSteps = targetPath.get("path_steps").arr.map(_.str)
        pathSteps should contain("usr/lib/lua/luci/controller/api/xqnetwork.luac::root.93@pc28:r8")
        pathSteps.exists(_.startsWith("usr/lib/lua/xiaoqiang/util/XQSysUtil.luac::")) shouldBe true
        pathSteps should contain("usr/lib/lua/xiaoqiang/util/XQSynchrodata.luac::root.0@pc25:r4")
        pathSteps.foreach(_ should include("::"))
        targetPath.get.obj.contains("callsite_id") shouldBe false
      }
    }

    "export CrossPlatform r7 XQSynchrodata synthetic report-facing function identity" in {
      withXiaomiStagingRows { stagingRows =>
        val synchrodata = stagingRows
          .find(_("relative_path").str.endsWith("usr/lib/lua/xiaoqiang/util/XQSynchrodata.luac"))
          .getOrElse(fail("missing XQSynchrodata staging evidence"))

        val identityRows = synchrodata("function_identity").arr.map(_.obj)
        identityRows.exists(row =>
          row("module_path").str == "usr/lib/lua/xiaoqiang/util/XQSynchrodata.luac" &&
            row("prototype_id").str == "root.0" &&
            row("display_name").str == "func_unknow_0_0" &&
            row("identity_kind").str == "synthetic" &&
            row("provenance").str == "upstream-lua2cpg,bytecode-only,synthetic-name"
        ) shouldBe true
      }
    }

    "export CrossPlatform r7 XQWifiUtil to XQSynchrodata strict producer evidence" in {
      withXiaomiStagingRows { stagingRows =>
        val wifiUtil = stagingRows
          .find(_("relative_path").str.endsWith("usr/lib/lua/xiaoqiang/util/XQWifiUtil.luac"))
          .getOrElse(fail("missing XQWifiUtil staging evidence"))

        val linkageRows = wifiUtil("module_linkage").arr.map(_.obj)
        linkageRows.exists(row =>
          row("callsite_id").str == "usr/lib/lua/xiaoqiang/util/XQWifiUtil.luac::root.41@pc225" &&
            row("target_module_path").str == "usr/lib/lua/xiaoqiang/util/XQSynchrodata.luac" &&
            row("target_prototype_id").str == "root.3" &&
            row("field_name").str == "syncWiFiSSID" &&
            row("resolution_status").str == "matched"
        ) shouldBe true

        val argRows = wifiUtil("interproc_arg_flow").arr.map(_.obj)
        argRows.exists(row =>
          row("callsite_id").str == "usr/lib/lua/xiaoqiang/util/XQWifiUtil.luac::root.41@pc225" &&
            row("from_argument_ref").str == "usr/lib/lua/xiaoqiang/util/XQWifiUtil.luac:root.41@pc225:r22" &&
            row("argument_index").num.toInt == 1 &&
            row("target_module_path").str == "usr/lib/lua/xiaoqiang/util/XQSynchrodata.luac" &&
            row("target_prototype_id").str == "root.3" &&
            row("to_parameter_ref").str == "usr/lib/lua/xiaoqiang/util/XQSynchrodata.luac:root.3:r1"
        ) shouldBe true
      }
    }

    "export CrossPlatform r7 requestMitv paths through referenceAnalyzer-equivalent call result flow" in {
      withXiaomiStagingRows { stagingRows =>
        val pathRows = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        def requestMitvPath(sinkModule: String, sinkFunction: String, sinkPc: Int, sinkTrigger: String) =
          pathRows.find(row =>
            row("source_module_path").str == "usr/lib/lua/luci/controller/api/xqsmarthome.luac" &&
              row("source_function_name").str == "requestMitv" &&
              row("source_pc").num.toInt == 3 &&
              row("source_trigger").str == "luci.http.formvalue" &&
              row("sink_module_path").str == sinkModule &&
              row("sink_function_name").str == sinkFunction &&
              row("sink_pc").num.toInt == sinkPc &&
              row("sink_trigger").str == sinkTrigger &&
              row("path_steps").arr.nonEmpty &&
              row("path_steps").arr.forall(_.str.contains("::")) &&
              !row.obj.contains("callsite_id")
          )

        val doExecPath = requestMitvPath(
          "usr/lib/lua/xiaoqiang/util/XQMitvUtil.luac",
          "DoExec",
          10,
          "luci.util.exec"
        ).getOrElse(fail("missing requestMitv to XQMitvUtil.DoExec strict path"))
        val doExecSteps = doExecPath("path_steps").arr.map(_.str)

        doExecSteps should contain("usr/lib/lua/luci/controller/api/xqsmarthome.luac::root.5@pc3:r0")
        doExecSteps should contain("usr/lib/lua/luci/controller/api/xqsmarthome.luac::root.5@pc11:r4")
        doExecSteps should contain("usr/lib/lua/xiaoqiang/util/XQMitvUtil.luac::root.1:r0")
        doExecSteps should contain("usr/lib/lua/xiaoqiang/util/XQMitvUtil.luac::root.1@pc7:r2")
        doExecSteps should contain("usr/lib/lua/xiaoqiang/util/XQMitvUtil.luac::root.1@pc7:r1")
        doExecSteps.exists(_.startsWith("usr/lib/lua/xiaoqiang/util/XQMitvUtil.luac::root.0")) shouldBe true
        doExecSteps should contain("usr/lib/lua/xiaoqiang/util/XQMitvUtil.luac::root.0@pc10:r3")
        assertRequestMitvStringMatchSanitizer(doExecPath)

        val popenPath = requestMitvPath(
          "usr/lib/lua/luci/util.luac",
          "exec",
          3,
          "io.popen"
        ).getOrElse(fail("missing requestMitv to luci.util.exec strict path"))
        val popenSteps = popenPath("path_steps").arr.map(_.str)

        popenSteps should contain("usr/lib/lua/luci/controller/api/xqsmarthome.luac::root.5@pc3:r0")
        popenSteps should contain("usr/lib/lua/xiaoqiang/util/XQMitvUtil.luac::root.1@pc7:r2")
        popenSteps should contain("usr/lib/lua/xiaoqiang/util/XQMitvUtil.luac::root.1@pc7:r1")
        popenSteps should contain("usr/lib/lua/xiaoqiang/util/XQMitvUtil.luac::root.0@pc10:r3")
        popenSteps should contain("usr/lib/lua/luci/util.luac::root.36:r0")
        popenSteps should contain("usr/lib/lua/luci/util.luac::root.36@pc3:r2")
        assertRequestMitvStringMatchSanitizer(popenPath)
      }
    }

    "export CrossPlatform r7 pppoeStatus path to luci util exec" in {
      withXiaomiStagingRows { stagingRows =>
        val pathRows = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        val path = pathRows
          .find(row =>
            row("source_module_path").str == "usr/lib/lua/luci/controller/api/xqnetwork.luac" &&
              row("source_function_name").str == "pppoeStatus" &&
              row("source_pc").num.toInt == 6 &&
              row("source_trigger").str == "luci.http.formvalue" &&
              row("sink_module_path").str == "usr/lib/lua/luci/util.luac" &&
              row("sink_function_name").str == "exec" &&
              row("sink_pc").num.toInt == 3 &&
              row("sink_trigger").str == "io.popen" &&
              row("path_steps").arr.nonEmpty &&
              row("path_steps").arr.forall(_.str.contains("::")) &&
              !row.obj.contains("callsite_id")
          )
          .getOrElse(fail("missing pppoeStatus to luci.util.exec strict path"))

        val steps = path("path_steps").arr.map(_.str)
        steps should contain("usr/lib/lua/luci/controller/api/xqnetwork.luac::root.55@pc6:r1")
        steps should contain("usr/lib/lua/luci/util.luac::root.36:r0")
        steps should contain("usr/lib/lua/luci/util.luac::root.36@pc3:r2")
      }
    }

    "export CrossPlatform r7 miats getWifiMacfilterInfo path to luci util exec" in {
      withXiaomiStagingRows { stagingRows =>
        val pathRows = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        val path = pathRows
          .find(row =>
            row("source_module_path").str == "usr/lib/lua/luci/controller/api/miats.luac" &&
              row("source_function_name").str == "getWifiMacfilterInfo" &&
              row("source_pc").num.toInt == 70 &&
              row("source_trigger").str == "luci.http.formvalue" &&
              row("sink_module_path").str == "usr/lib/lua/luci/util.luac" &&
              row("sink_function_name").str == "exec" &&
              row("sink_pc").num.toInt == 3 &&
              row("sink_trigger").str == "io.popen" &&
              row("path_steps").arr.nonEmpty &&
              row("path_steps").arr.forall(_.str.contains("::")) &&
              !row.obj.contains("callsite_id")
          )
          .getOrElse(fail("missing miats.getWifiMacfilterInfo to luci.util.exec strict path"))

        val steps = path("path_steps").arr.map(_.str)
        steps should contain("usr/lib/lua/luci/controller/api/miats.luac::root.3@pc70:r7")
        steps should contain("usr/lib/lua/luci/util.luac::root.36:r0")
        steps should contain("usr/lib/lua/luci/util.luac::root.36@pc3:r2")
      }
    }

    "export CrossPlatform r7 vpnSwitch path to XQCryptoUtil md5Str" in {
      withXiaomiStagingRows { stagingRows =>
        val pathRows = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        val path = pathRows
          .find(row =>
            row("source_module_path").str == "usr/lib/lua/luci/controller/api/xqsystem.luac" &&
              row("source_function_name").str == "vpnSwitch" &&
              row("source_pc").num.toInt == 12 &&
              row("source_trigger").str == "luci.http.formvalue" &&
              row("sink_module_path").str == "usr/lib/lua/xiaoqiang/util/XQCryptoUtil.luac" &&
              row("sink_function_name").str == "md5Str" &&
              row("sink_pc").num.toInt == 10 &&
              row("sink_trigger").str == "luci.util.exec" &&
              row("path_steps").arr.nonEmpty &&
              row("path_steps").arr.forall(_.str.contains("::")) &&
              !row.obj.contains("callsite_id")
          )
          .getOrElse(fail("missing vpnSwitch to XQCryptoUtil.md5Str strict path"))

        val steps = path("path_steps").arr.map(_.str)
        steps should contain("usr/lib/lua/luci/controller/api/xqsystem.luac::root.92@pc12:r2")
        steps should contain("usr/lib/lua/xiaoqiang/util/XQCryptoUtil.luac::root.3@pc10:r4")
      }
    }

    "export CrossPlatform r7 editDevice path to XQWifiUtil wl_editWiFiMacfilterList" in {
      withXiaomiExportDir { exportDir =>
        val profile = ujson.read(Files.readString(exportDir.resolve("path-search-profile.json"))).obj
        profile("report_count").num.toInt should be <= 4000
        profile("local_path_search_count").num.toInt should be <= 30000

        val stagingDir    = exportDir.resolve("staging")
        val stagingStream = Files.list(stagingDir)
        val stagingRows   = stagingStream.iterator.asScala.toVector.map(path => ujson.read(Files.readString(path)).obj)
        try {
        val pathRows = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        val path = pathRows
          .find(row =>
            row("source_module_path").str == "usr/lib/lua/luci/controller/api/xqnetwork.luac" &&
              row("source_function_name").str == "editDevice" &&
              row("source_pc").num.toInt == 20 &&
              row("source_trigger").str == "luci.http.formvalue" &&
              row("sink_module_path").str == "usr/lib/lua/xiaoqiang/util/XQWifiUtil.luac" &&
              row("sink_function_name").str == "wl_editWiFiMacfilterList" &&
              row("sink_pc").num.toInt == 348 &&
              row("sink_trigger").str == "os.execute" &&
              row("path_steps").arr.nonEmpty &&
              row("path_steps").arr.forall(_.str.contains("::")) &&
              !row.obj.contains("callsite_id")
          )
          .getOrElse(fail("missing editDevice to XQWifiUtil.wl_editWiFiMacfilterList strict path"))

        val steps = path("path_steps").arr.map(_.str)
        steps should contain("usr/lib/lua/luci/controller/api/xqnetwork.luac::root.48@pc20:r7")
        steps should contain("usr/lib/lua/xiaoqiang/util/XQWifiUtil.luac::root.82@pc348:r17")
        } finally {
          stagingStream.close()
        }
      }
    }

    "export CrossPlatform r7 editDevice path with referenceAnalyzer known _cmdformat sanitizer hit" in {
      withXiaomiStagingRows { stagingRows =>
        val pathRows = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        val sourceModule    = "usr/lib/lua/luci/controller/api/xqnetwork.luac"
        val sinkModule      = "usr/lib/lua/xiaoqiang/util/XQWifiUtil.luac"
        val sanitizerModule = "usr/lib/lua/xiaoqiang/common/XQFunction.luac"
        val sanitizerCall   = s"$sanitizerModule::root.0@pc12"
        val sanitizerValue  = s"$sanitizerCall:r1"

        val path = pathRows
          .find(row =>
            row("source_module_path").str == sourceModule &&
              row("source_function_name").str == "editDevice" &&
              row("source_pc").num.toInt == 20 &&
              row("source_trigger").str == "luci.http.formvalue" &&
              row("sink_module_path").str == sinkModule &&
              row("sink_function_name").str == "wl_editWiFiMacfilterList" &&
              row("sink_pc").num.toInt == 348 &&
              row("sink_trigger").str == "os.execute" &&
              row("path_steps").arr.nonEmpty &&
              row("path_steps").arr.forall(_.str.contains("::")) &&
              row("path_steps").arr.exists(_.str == sanitizerValue) &&
              !row.obj.contains("callsite_id")
          )
          .getOrElse(fail("missing editDevice to wl_editWiFiMacfilterList strict path"))

        withClue(
          s"path_steps=${path("path_steps").arr.map(_.str).mkString("[", ",", "]")} sanitizer_hits=${path("sanitizer_hits")}"
        ) {
          path("classification").str shouldBe "sanitized"
          path("sanitizer_hits").arr.exists { hit =>
            val row = hit.obj
            row("callsite_id").str == sanitizerCall &&
            row("callsite_id").str.contains("::") &&
            row("sanitizer_name").str == "_cmdformat" &&
            row("applies_to_sink").bool &&
            row("on_dataflow_chain").bool
          } shouldBe true
        }
      }
    }

    "export CrossPlatform r7 setConfigIotDevHidessid paths with referenceAnalyzer known _cmdformat sanitizer hits" in {
      withXiaomiStagingRows { stagingRows =>
        val pathRows = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        val module = "usr/lib/lua/luci/controller/api/misystem.luac"
        val expectedPairs = Vector(
          11 -> 161,
          11 -> 186,
          19 -> 161,
          19 -> 186,
          23 -> 161,
          23 -> 186,
          27 -> 161,
          27 -> 186,
          31 -> 186,
          35 -> 186
        )

        expectedPairs.foreach { case (sourcePc, sinkPc) =>
          val path = pathRows
            .find(row =>
              row("source_module_path").str == module &&
                row("source_function_name").str == "setConfigIotDevHidessid" &&
                row("source_pc").num.toInt == sourcePc &&
                row("source_trigger").str == "luci.http.formvalue" &&
                row("sink_module_path").str == module &&
                row("sink_function_name").str == "setConfigIotDevHidessid" &&
                row("sink_pc").num.toInt == sinkPc &&
                row("sink_trigger").str == "test.api.Process.forkExec" &&
                row("path_steps").arr.nonEmpty &&
                row("path_steps").arr.forall(_.str.contains("::")) &&
                !row.obj.contains("callsite_id")
            )
            .getOrElse(fail(s"missing setConfigIotDevHidessid strict path sourcePc=$sourcePc sinkPc=$sinkPc"))

          withClue(
            s"sourcePc=$sourcePc sinkPc=$sinkPc path_steps=${path("path_steps").arr.map(_.str).mkString("[", ",", "]")} sanitizer_hits=${path("sanitizer_hits")}"
          ) {
            path("classification").str shouldBe "sanitized"
            path("sanitizer_hits").arr.exists { hit =>
              val row = hit.obj
              row("callsite_id").str.startsWith(s"$module::root.172@pc") &&
              row("callsite_id").str.contains("::") &&
              row("sanitizer_name").str == "test.api.Process._cmdformat" &&
              row("applies_to_sink").bool &&
              row("on_dataflow_chain").bool
            } shouldBe true
          }
        }
      }
    }

    "export CrossPlatform r7 addMeshNode paths with referenceAnalyzer known _strformat sanitizer hits" in {
      withXiaomiStagingRows { stagingRows =>
        val pathRows = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        val sourceModule = "usr/lib/lua/luci/controller/api/xqnetwork.luac"
        val sinkModule   = "usr/lib/lua/xiaoqiang/util/XQWifiUtil.luac"
        val expectedPairs = Vector(
          (11, 16, 15),
          (11, 32, 31),
          (15, 32, 31)
        )

        expectedPairs.foreach { case (sourcePc, sinkPc, sanitizerPc) =>
          val path = pathRows
            .find(row =>
              row("source_module_path").str == sourceModule &&
                row("source_function_name").str == "addMeshNode" &&
                row("source_pc").num.toInt == sourcePc &&
                row("source_trigger").str == "luci.http.formvalue" &&
                row("sink_module_path").str == sinkModule &&
                row("sink_function_name").str == "mesh_add_node" &&
                row("sink_pc").num.toInt == sinkPc &&
                row("sink_trigger").str == "test.api.Process.forkExec" &&
                row("path_steps").arr.nonEmpty &&
                row("path_steps").arr.forall(_.str.contains("::")) &&
                !row.obj.contains("callsite_id")
            )
            .getOrElse(fail(s"missing addMeshNode strict path sourcePc=$sourcePc sinkPc=$sinkPc"))

          withClue(
            s"sourcePc=$sourcePc sinkPc=$sinkPc path_steps=${path("path_steps").arr.map(_.str).mkString("[", ",", "]")} sanitizer_hits=${path("sanitizer_hits")}"
          ) {
            path("classification").str shouldBe "sanitized"
            path("sanitizer_hits").arr.exists { hit =>
              val row = hit.obj
              row("callsite_id").str == s"$sinkModule::root.101@pc$sanitizerPc" &&
              row("sanitizer_name").str == "test.api.Process._strformat" &&
              row("applies_to_sink").bool &&
              row("on_dataflow_chain").bool
            } shouldBe true
          }
        }
      }
    }

    "preserve CrossPlatform r7 baseline sanitizer classifications for setWifiMacfilter and setWanSpeed" in {
      withXiaomiStagingRows { stagingRows =>
        val pathRows = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        val wifiPath = pathRows
          .find(row =>
            row("source_module_path").str == "usr/lib/lua/luci/controller/api/xqnetwork.luac" &&
              row("source_function_name").str == "setWifiMacfilter" &&
              row("source_pc").num.toInt == 34 &&
              row("sink_module_path").str == "usr/lib/lua/xiaoqiang/util/XQSynchrodata.luac" &&
              row("sink_function_name").str == "func_unknow_0_0" &&
              row("sink_pc").num.toInt == 25 &&
              row("sink_trigger").str == "os.execute"
          )
          .getOrElse(fail("missing setWifiMacfilter to XQSynchrodata.func_unknow_0_0 strict path"))

        wifiPath("classification").str shouldBe "sanitized"
        wifiPath("sanitizer_hits").arr.exists { hit =>
          val row = hit.obj
          row("callsite_id").str == "usr/lib/lua/luci/controller/api/xqnetwork.luac::root.47@pc35" &&
          row("sanitizer_name").str == "tonumber" &&
          row("applies_to_sink").bool &&
          row("on_dataflow_chain").bool
        } shouldBe true

        val wanSpeedPath = pathRows
          .find(row =>
            row("source_module_path").str == "usr/lib/lua/luci/controller/api/xqnetwork.luac" &&
              row("source_function_name").str == "setWanSpeed" &&
              row("source_pc").num.toInt == 7 &&
              row("sink_module_path").str == "usr/lib/lua/xiaoqiang/util/XQLanWanUtil.luac" &&
              row("sink_function_name").str == "setWanSpeed" &&
              row("sink_pc").num.toInt == 31 &&
              row("sink_trigger").str == "os.execute"
          )
          .getOrElse(fail("missing setWanSpeed to XQLanWanUtil.setWanSpeed strict path"))

        wanSpeedPath("classification").str shouldBe "sanitized"
        wanSpeedPath("sanitizer_hits").arr.exists { hit =>
          val row = hit.obj
          row("callsite_id").str == "usr/lib/lua/luci/controller/api/xqnetwork.luac::root.82@pc8" &&
          row("sanitizer_name").str == "tonumber" &&
          row("applies_to_sink").bool &&
          row("on_dataflow_chain").bool
        } shouldBe true
        wanSpeedPath("sanitizer_hits").arr.exists { hit =>
          val row = hit.obj
          row("callsite_id").str == "usr/lib/lua/xiaoqiang/util/XQLanWanUtil.luac::root.68@pc5" &&
          row("sanitizer_name").str == "tonumber" &&
          row("applies_to_sink").bool &&
          row("on_dataflow_chain").bool
        } shouldBe true
      }
    }

    "export CrossPlatform r7 setAllWifi path through conditional call result flow" in {
      withXiaomiStagingRows { stagingRows =>
        val pathRows = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        val path = pathRows
          .find(row =>
            row("source_module_path").str == "usr/lib/lua/luci/controller/api/xqnetwork.luac" &&
              row("source_function_name").str == "setAllWifi" &&
              row("source_pc").num.toInt == 40 &&
              row("source_trigger").str == "luci.http.formvalue" &&
              row("sink_module_path").str == "usr/lib/lua/xiaoqiang/common/XQFunction.luac" &&
              row("sink_function_name").str == "nvramSet" &&
              row("sink_pc").num.toInt == 35 &&
              row("sink_trigger").str == "os.execute" &&
              row("path_steps").arr.nonEmpty &&
              row("path_steps").arr.forall(_.str.contains("::")) &&
              !row.obj.contains("callsite_id")
          )
          .getOrElse(fail("missing setAllWifi to XQFunction.nvramSet strict path"))

        val steps = path("path_steps").arr.map(_.str)
        steps should contain("usr/lib/lua/luci/controller/api/xqnetwork.luac::root.15@pc40:r13")
        steps should contain("usr/lib/lua/luci/controller/api/xqnetwork.luac::root.15@pc287:r13")
        steps should contain("usr/lib/lua/luci/controller/api/xqnetwork.luac::root.15@pc287:r48")
        steps should contain("usr/lib/lua/luci/controller/api/xqnetwork.luac::root.15@pc300:r48")
        steps should contain("usr/lib/lua/xiaoqiang/util/XQWifiUtil.luac::root.41:r2")
        steps should contain("usr/lib/lua/xiaoqiang/common/XQFunction.luac::root.33@pc35:r4")
      }
    }

    "export CrossPlatform r8 setAllWifi formvalue paths to XQFunction nvramSet" in {
      withXiaomiStagingRows { stagingRows =>
        val sourceRows = stagingRows.flatMap(_("source_endpoints").arr.map(_.obj))
        val sinkRows   = stagingRows.flatMap(_("sink_endpoints").arr.map(_.obj))
        val pathRows   = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        val sourceModule = "usr/lib/lua/luci/controller/api/xqnetwork.luac"
        val sinkModule   = "usr/lib/lua/xiaoqiang/common/XQFunction.luac"
        val expectedSources = Vector(
          74  -> "root.15@pc74:r20",
          78  -> "root.15@pc78:r21",
          85  -> "root.15@pc85:r22",
          112 -> "root.15@pc112:r28",
          116 -> "root.15@pc116:r29",
          120 -> "root.15@pc120:r30"
        )

        expectedSources.foreach { case (pc, localRef) =>
          sourceRows.exists(row =>
            row("module_path").str == sourceModule &&
              row("value_ref").str == localRef &&
              hasScopedCallsite(row, s"root.15@pc$pc") &&
              row("trigger").str == "luci.http.formvalue"
          ) shouldBe true
        }

        sinkRows.exists(row =>
          row("module_path").str == sinkModule &&
            row("value_ref").str == "root.33@pc35:r4" &&
            hasScopedCallsite(row, "root.33@pc35") &&
            row("trigger").str == "os.execute"
        ) shouldBe true

        val missingPaths = expectedSources.collect { case (pc, localRef)
            if !pathRows.exists(row =>
              row("source_module_path").str == sourceModule &&
                row("source_function_name").str == "setAllWifi" &&
                row("source_pc").num.toInt == pc &&
                row("source_trigger").str == "luci.http.formvalue" &&
                row("sink_module_path").str == sinkModule &&
                row("sink_function_name").str == "nvramSet" &&
                row("sink_pc").num.toInt == 35 &&
                row("sink_trigger").str == "os.execute" &&
                row("path_steps").arr.nonEmpty &&
                row("path_steps").arr.forall(_.str.contains("::")) &&
                row("path_steps").arr.exists(_.str == s"$sourceModule::$localRef") &&
                row("path_steps").arr.exists(_.str == s"$sinkModule::root.33@pc35:r4") &&
                !row.obj.contains("callsite_id")
            ) =>
          s"setAllWifi@pc$pc"
        }

        withClue(s"missing r8 paths: ${missingPaths.mkString(", ")}") {
          missingPaths shouldBe empty
        }
      }
    }

    "export CrossPlatform r8 setRouterInfo formvalue paths to XQFunction nvramSet" in {
      withXiaomiStagingRows { stagingRows =>
        val sourceRows = stagingRows.flatMap(_("source_endpoints").arr.map(_.obj))
        val sinkRows   = stagingRows.flatMap(_("sink_endpoints").arr.map(_.obj))
        val pathRows   = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        val sourceModule = "usr/lib/lua/luci/controller/api/misystem.luac"
        val sinkModule   = "usr/lib/lua/xiaoqiang/common/XQFunction.luac"
        val expectedSources = Vector(
          74 -> "root.27@pc74:r17",
          86 -> "root.27@pc86:r20",
          90 -> "root.27@pc90:r21"
        )

        expectedSources.foreach { case (pc, localRef) =>
          sourceRows.exists(row =>
            row("module_path").str == sourceModule &&
              row("value_ref").str == localRef &&
              hasScopedCallsite(row, s"root.27@pc$pc") &&
              row("trigger").str == "luci.http.formvalue"
          ) shouldBe true
        }

        sinkRows.exists(row =>
          row("module_path").str == sinkModule &&
            row("value_ref").str == "root.33@pc35:r4" &&
            hasScopedCallsite(row, "root.33@pc35") &&
            row("trigger").str == "os.execute"
        ) shouldBe true

        val missingPaths = expectedSources.collect { case (pc, localRef)
            if !pathRows.exists(row =>
              row("source_module_path").str == sourceModule &&
                row("source_function_name").str == "setRouterInfo" &&
                row("source_pc").num.toInt == pc &&
                row("source_trigger").str == "luci.http.formvalue" &&
                row("sink_module_path").str == sinkModule &&
                row("sink_function_name").str == "nvramSet" &&
                row("sink_pc").num.toInt == 35 &&
                row("sink_trigger").str == "os.execute" &&
                row("path_steps").arr.nonEmpty &&
                row("path_steps").arr.forall(_.str.contains("::")) &&
                row("path_steps").arr.exists(_.str == s"$sourceModule::$localRef") &&
                row("path_steps").arr.exists(_.str == s"$sinkModule::root.33@pc35:r4") &&
                !row.obj.contains("callsite_id")
            ) =>
          s"setRouterInfo@pc$pc"
        }

        withClue(s"missing r8 paths: ${missingPaths.mkString(", ")}") {
          missingPaths shouldBe empty
        }
      }
    }

    "export CrossPlatform r8 misystem paths to XQSynchrodata" in {
      withXiaomiStagingRows { stagingRows =>
        val sourceRows = stagingRows.flatMap(_("source_endpoints").arr.map(_.obj))
        val sinkRows   = stagingRows.flatMap(_("sink_endpoints").arr.map(_.obj))
        val pathRows   = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        val sourceModule = "usr/lib/lua/luci/controller/api/misystem.luac"
        val sinkModule   = "usr/lib/lua/xiaoqiang/util/XQSynchrodata.luac"
        val expectedSources = Vector(
          ("setRouterInfo", 78, "root.27@pc78", "root.27@pc78:r18"),
          ("setLanApMode_Init", 92, "root.40@pc92", "root.40@pc92:r19")
        )

        expectedSources.foreach { case (_, _, callsiteId, localRef) =>
          sourceRows.exists(row =>
            row("module_path").str == sourceModule &&
              row("value_ref").str == localRef &&
              hasScopedCallsite(row, callsiteId) &&
              row("trigger").str == "luci.http.formvalue"
          ) shouldBe true
        }

        sinkRows.exists(row =>
          row("module_path").str == sinkModule &&
            row("value_ref").str == "root.0@pc25:r4" &&
            hasScopedCallsite(row, "root.0@pc25") &&
            row("trigger").str == "os.execute"
        ) shouldBe true

        val missingPaths = expectedSources.collect { case (functionName, pc, _, localRef)
            if !pathRows.exists(row =>
              row("source_module_path").str == sourceModule &&
                row("source_function_name").str == functionName &&
                row("source_pc").num.toInt == pc &&
                row("source_trigger").str == "luci.http.formvalue" &&
                row("sink_module_path").str == sinkModule &&
                row("sink_function_name").str == "func_unknow_0_0" &&
                row("sink_pc").num.toInt == 25 &&
                row("sink_trigger").str == "os.execute" &&
                row("path_steps").arr.nonEmpty &&
                row("path_steps").arr.forall(_.str.contains("::")) &&
                row("path_steps").arr.exists(_.str == s"$sourceModule::$localRef") &&
                row("path_steps").arr.exists(_.str == s"$sinkModule::root.0@pc25:r4") &&
                !row.obj.contains("callsite_id")
            ) =>
          s"$functionName@pc$pc"
        }

        withClue(s"missing r8 XQSynchrodata paths: ${missingPaths.mkString(", ")}") {
          missingPaths shouldBe empty
        }
      }
    }

    "export CrossPlatform r8 setRouterToBaidu formvalue paths to local exec sink" in {
      withXiaomiStagingRows { stagingRows =>
        val sourceRows = stagingRows.flatMap(_("source_endpoints").arr.map(_.obj))
        val sinkRows   = stagingRows.flatMap(_("sink_endpoints").arr.map(_.obj))
        val pathRows   = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        val module = "usr/lib/lua/luci/controller/api/xqnetwork.luac"
        val expectedSources = Vector(
          27 -> "root.131@pc27:r8",
          35 -> "root.131@pc35:r10"
        )

        expectedSources.foreach { case (pc, localRef) =>
          sourceRows.exists(row =>
            row("module_path").str == module &&
              row("value_ref").str == localRef &&
              hasScopedCallsite(row, s"root.131@pc$pc") &&
              row("trigger").str == "luci.http.formvalue"
          ) shouldBe true
        }

        sinkRows.exists(row =>
          row("module_path").str == module &&
            row("value_ref").str == "root.131@pc161:r19" &&
            hasScopedCallsite(row, "root.131@pc161") &&
            row("trigger").str == "luci.util.exec"
        ) shouldBe true

        val missingPaths = expectedSources.collect { case (pc, localRef)
            if !pathRows.exists(row =>
              row("source_module_path").str == module &&
                row("source_function_name").str == "setRouterToBaidu" &&
                row("source_pc").num.toInt == pc &&
                row("source_trigger").str == "luci.http.formvalue" &&
                row("sink_module_path").str == module &&
                row("sink_function_name").str == "setRouterToBaidu" &&
                row("sink_pc").num.toInt == 161 &&
                row("sink_trigger").str == "luci.util.exec" &&
                row("path_steps").arr.nonEmpty &&
                row("path_steps").arr.forall(_.str.contains("::")) &&
                row("path_steps").arr.exists(_.str == s"$module::$localRef") &&
                row("path_steps").arr.exists(_.str == s"$module::root.131@pc161:r19") &&
                !row.obj.contains("callsite_id")
            ) =>
          s"setRouterToBaidu@pc$pc"
        }

        withClue(s"missing r8 setRouterToBaidu local exec paths: ${missingPaths.mkString(", ")}") {
          missingPaths shouldBe empty
        }
      }
    }

    "export CrossPlatform r8 setBaiduToRouter formvalue path to local exec sink" in {
      withXiaomiStagingRows { stagingRows =>
        val sourceRows = stagingRows.flatMap(_("source_endpoints").arr.map(_.obj))
        val sinkRows   = stagingRows.flatMap(_("sink_endpoints").arr.map(_.obj))
        val pathRows   = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        val module    = "usr/lib/lua/luci/controller/api/xqnetwork.luac"
        val sourceRef = "root.132@pc27:r8"
        val sinkRef   = "root.132@pc116:r16"

        sourceRows.exists(row =>
          row("module_path").str == module &&
            row("value_ref").str == sourceRef &&
            hasScopedCallsite(row, "root.132@pc27") &&
            row("trigger").str == "luci.http.formvalue"
        ) shouldBe true

        sinkRows.exists(row =>
          row("module_path").str == module &&
            row("value_ref").str == sinkRef &&
            hasScopedCallsite(row, "root.132@pc116") &&
            row("trigger").str == "luci.util.exec"
        ) shouldBe true

        val pathExists = pathRows.exists(row =>
          row("source_module_path").str == module &&
            row("source_function_name").str == "setBaiduToRouter" &&
            row("source_pc").num.toInt == 27 &&
            row("source_trigger").str == "luci.http.formvalue" &&
            row("sink_module_path").str == module &&
            row("sink_function_name").str == "setBaiduToRouter" &&
            row("sink_pc").num.toInt == 116 &&
            row("sink_trigger").str == "luci.util.exec" &&
            row("path_steps").arr.nonEmpty &&
            row("path_steps").arr.forall(_.str.contains("::")) &&
            row("path_steps").arr.exists(_.str == s"$module::$sourceRef") &&
            row("path_steps").arr.exists(_.str == s"$module::$sinkRef") &&
            !row.obj.contains("callsite_id")
        )

        withClue("missing r8 setBaiduToRouter local exec path") {
          pathExists shouldBe true
        }
      }
    }

    "export CrossPlatform r8 getTransListFileStat sanitized path to local exec sink" in {
      withXiaomiStagingRows { stagingRows =>
        val sourceRows = stagingRows.flatMap(_("source_endpoints").arr.map(_.obj))
        val sinkRows   = stagingRows.flatMap(_("sink_endpoints").arr.map(_.obj))
        val pathRows   = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        val module    = "usr/lib/lua/luci/controller/api/xqnetwork.luac"
        val sourceRef = "root.137@pc26:r7"
        val sinkRef   = "root.137@pc68:r14"

        sourceRows.exists(row =>
          row("module_path").str == module &&
            row("value_ref").str == sourceRef &&
            hasScopedCallsite(row, "root.137@pc26") &&
            row("trigger").str == "luci.http.formvalue"
        ) shouldBe true

        sinkRows.exists(row =>
          row("module_path").str == module &&
            row("value_ref").str == sinkRef &&
            hasScopedCallsite(row, "root.137@pc68") &&
            row("trigger").str == "luci.util.exec"
        ) shouldBe true

        val path = pathRows
          .find(row =>
            row("source_module_path").str == module &&
              row("source_function_name").str == "getTransListFileStat" &&
              row("source_pc").num.toInt == 26 &&
              row("source_trigger").str == "luci.http.formvalue" &&
              row("sink_module_path").str == module &&
              row("sink_function_name").str == "getTransListFileStat" &&
              row("sink_pc").num.toInt == 68 &&
              row("sink_trigger").str == "luci.util.exec" &&
              row("path_steps").arr.nonEmpty &&
              row("path_steps").arr.forall(_.str.contains("::")) &&
              row("path_steps").arr.exists(_.str == s"$module::$sourceRef") &&
              row("path_steps").arr.exists(_.str == s"$module::$sinkRef") &&
              !row.obj.contains("callsite_id")
          )
          .getOrElse(fail("missing r8 getTransListFileStat local exec path"))

        withClue(
          s"path_steps=${path("path_steps").arr.map(_.str).mkString("[", ",", "]")} sanitizer_hits=${path("sanitizer_hits")}"
        ) {
          path("classification").str shouldBe "sanitized"
          path("sanitizer_hits").arr.exists { hit =>
            val row = hit.obj
            row("callsite_id").str == s"$module::root.137@pc60" &&
            row("sanitizer_name").str == "json.encode" &&
            row("applies_to_sink").bool &&
            row("on_dataflow_chain").bool
          } shouldBe true
        }
      }
    }

    "export CrossPlatform r8 tunnelSmartHomeRequest formvalue path to local exec sink" in {
      withXiaomiStagingRows { stagingRows =>
        val sourceRows = stagingRows.flatMap(_("source_endpoints").arr.map(_.obj))
        val sinkRows   = stagingRows.flatMap(_("sink_endpoints").arr.map(_.obj))
        val pathRows   = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        val module    = "usr/lib/lua/luci/controller/api/xqsmarthome.luac"
        val sourceRef = "root.1@pc7:r2"
        val sinkRef   = "root.1@pc19:r6"

        sourceRows.exists(row =>
          row("module_path").str == module &&
            row("value_ref").str == sourceRef &&
            hasScopedCallsite(row, "root.1@pc7") &&
            row("trigger").str == "luci.http.formvalue"
        ) shouldBe true

        sinkRows.exists(row =>
          row("module_path").str == module &&
            row("value_ref").str == sinkRef &&
            hasScopedCallsite(row, "root.1@pc19") &&
            row("trigger").str == "luci.util.exec"
        ) shouldBe true

        val pathExists = pathRows.exists(row =>
          row("source_module_path").str == module &&
            row("source_function_name").str == "tunnelSmartHomeRequest" &&
            row("source_pc").num.toInt == 7 &&
            row("source_trigger").str == "luci.http.formvalue" &&
            row("sink_module_path").str == module &&
            row("sink_function_name").str == "tunnelSmartHomeRequest" &&
            row("sink_pc").num.toInt == 19 &&
            row("sink_trigger").str == "luci.util.exec" &&
            row("classification").str == "true-positive" &&
            row("path_steps").arr.nonEmpty &&
            row("path_steps").arr.forall(_.str.contains("::")) &&
            row("path_steps").arr.exists(_.str == s"$module::$sourceRef") &&
            row("path_steps").arr.exists(_.str == s"$module::$sinkRef") &&
            !row.obj.contains("callsite_id")
        )

        withClue("missing r8 tunnelSmartHomeRequest local exec path") {
          pathExists shouldBe true
        }
      }
    }

    "export CrossPlatform r8 tunnelSmartControllerRequest formvalue path to local exec sink" in {
      withXiaomiStagingRows { stagingRows =>
        val sourceRows = stagingRows.flatMap(_("source_endpoints").arr.map(_.obj))
        val sinkRows   = stagingRows.flatMap(_("sink_endpoints").arr.map(_.obj))
        val pathRows   = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        val module    = "usr/lib/lua/luci/controller/api/xqsmarthome.luac"
        val sourceRef = "root.2@pc19:r5"
        val sinkRef   = "root.2@pc79:r10"

        sourceRows.exists(row =>
          row("module_path").str == module &&
            row("value_ref").str == sourceRef &&
            hasScopedCallsite(row, "root.2@pc19") &&
            row("trigger").str == "luci.http.formvalue"
        ) shouldBe true

        sinkRows.exists(row =>
          row("module_path").str == module &&
            row("value_ref").str == sinkRef &&
            hasScopedCallsite(row, "root.2@pc79") &&
            row("trigger").str == "luci.util.exec"
        ) shouldBe true

        val pathExists = pathRows.exists(row =>
          row("source_module_path").str == module &&
            row("source_function_name").str == "tunnelSmartControllerRequest" &&
            row("source_pc").num.toInt == 19 &&
            row("source_trigger").str == "luci.http.formvalue" &&
            row("sink_module_path").str == module &&
            row("sink_function_name").str == "tunnelSmartControllerRequest" &&
            row("sink_pc").num.toInt == 79 &&
            row("sink_trigger").str == "luci.util.exec" &&
            row("classification").str == "true-positive" &&
            row("path_steps").arr.nonEmpty &&
            row("path_steps").arr.forall(_.str.contains("::")) &&
            row("path_steps").arr.exists(_.str == s"$module::$sourceRef") &&
            row("path_steps").arr.exists(_.str == s"$module::$sinkRef") &&
            !row.obj.contains("callsite_id")
        )

        withClue("missing r8 tunnelSmartControllerRequest local exec path") {
          pathExists shouldBe true
        }
      }
    }

    "export CrossPlatform r8 setMeshInfo formvalue path to forkExec sink" in {
      withXiaomiStagingRows { stagingRows =>
        val sourceRows = stagingRows.flatMap(_("source_endpoints").arr.map(_.obj))
        val sinkRows   = stagingRows.flatMap(_("sink_endpoints").arr.map(_.obj))
        val pathRows   = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        val module    = "usr/lib/lua/luci/controller/api/misystem.luac"
        val sourceRef = "root.28@pc47:r12"
        val sinkRef   = "root.28@pc345:r23"

        sourceRows.exists(row =>
          row("module_path").str == module &&
            row("value_ref").str == sourceRef &&
            hasScopedCallsite(row, "root.28@pc47") &&
            row("trigger").str == "luci.http.formvalue"
        ) shouldBe true

        sinkRows.exists(row =>
          row("module_path").str == module &&
            row("value_ref").str == sinkRef &&
            hasScopedCallsite(row, "root.28@pc345") &&
            row("trigger").str == "test.api.Process.forkExec"
        ) shouldBe true

        val pathExists = pathRows.exists(row =>
          row("source_module_path").str == module &&
            row("source_function_name").str == "setMeshInfo" &&
            row("source_pc").num.toInt == 47 &&
            row("source_trigger").str == "luci.http.formvalue" &&
            row("sink_module_path").str == module &&
            row("sink_function_name").str == "setMeshInfo" &&
            row("sink_pc").num.toInt == 345 &&
            row("sink_trigger").str == "test.api.Process.forkExec" &&
            row("classification").str == "true-positive" &&
            row("path_steps").arr.nonEmpty &&
            row("path_steps").arr.forall(_.str.contains("::")) &&
            row("path_steps").arr.exists(_.str == s"$module::$sourceRef") &&
            row("path_steps").arr.exists(_.str == s"$module::$sinkRef") &&
            !row.obj.contains("callsite_id")
        )

        withClue("missing r8 setMeshInfo forkExec path") {
          pathExists shouldBe true
        }
      }
    }

    "export CrossPlatform r8 datacenter setSyncRouterFile path to tunnelRequestDatacenter exec sink" in {
      withXiaomiStagingRows { stagingRows =>
        val sourceRows = stagingRows.flatMap(_("source_endpoints").arr.map(_.obj))
        val sinkRows   = stagingRows.flatMap(_("sink_endpoints").arr.map(_.obj))
        val pathRows   = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        val module    = "usr/lib/lua/luci/controller/service/datacenter.luac"
        val sourceRef = "root.1@pc6:r1"
        val sinkRef   = "root.14@pc24:r7"

        sourceRows.exists(row =>
          row("module_path").str == module &&
            row("value_ref").str == sourceRef &&
            hasScopedCallsite(row, "root.1@pc6") &&
            row("trigger").str == "luci.http.formvalue"
        ) shouldBe true

        sinkRows.exists(row =>
          row("module_path").str == module &&
            row("value_ref").str == sinkRef &&
            hasScopedCallsite(row, "root.14@pc24") &&
            row("trigger").str == "luci.util.exec"
        ) shouldBe true

        val pathExists = pathRows.exists(row =>
          row("source_module_path").str == module &&
            row("source_function_name").str == "setSyncRouterFile" &&
            row("source_pc").num.toInt == 6 &&
            row("source_trigger").str == "luci.http.formvalue" &&
            row("sink_module_path").str == module &&
            row("sink_function_name").str == "tunnelRequestDatacenter" &&
            row("sink_pc").num.toInt == 24 &&
            row("sink_trigger").str == "luci.util.exec" &&
            row("classification").str == "true-positive" &&
            row("path_steps").arr.nonEmpty &&
            row("path_steps").arr.forall(_.str.contains("::")) &&
            row("path_steps").arr.exists(_.str == s"$module::$sourceRef") &&
            row("path_steps").arr.exists(_.str == s"$module::$sinkRef") &&
            !row.obj.contains("callsite_id")
        )

        withClue("missing r8 datacenter setSyncRouterFile local exec path") {
          pathExists shouldBe true
        }
      }
    }

    "export CrossPlatform r8 setSysTime path to XQSysUtil setSysTime forkExec sink" in {
      withXiaomiStagingRows { stagingRows =>
        val sourceRows = stagingRows.flatMap(_("source_endpoints").arr.map(_.obj))
        val sinkRows   = stagingRows.flatMap(_("sink_endpoints").arr.map(_.obj))
        val pathRows   = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        val sourceModule = "usr/lib/lua/luci/controller/api/misystem.luac"
        val sinkModule   = "usr/lib/lua/xiaoqiang/util/XQSysUtil.luac"
        val sourceRef    = "root.144@pc5:r1"
        val sinkRef      = "root.108@pc112:r4"

        sourceRows.exists(row =>
          row("module_path").str == sourceModule &&
            row("value_ref").str == sourceRef &&
            hasScopedCallsite(row, "root.144@pc5") &&
            row("trigger").str == "luci.http.formvalue"
        ) shouldBe true

        sinkRows.exists(row =>
          row("module_path").str == sinkModule &&
            row("value_ref").str == sinkRef &&
            hasScopedCallsite(row, "root.108@pc112") &&
            row("trigger").str == "test.api.Process.forkExec"
        ) shouldBe true

        val path = pathRows.find(row =>
          row("source_module_path").str == sourceModule &&
            row("source_function_name").str == "setSysTime" &&
            row("source_pc").num.toInt == 5 &&
            row("source_trigger").str == "luci.http.formvalue" &&
            row("sink_module_path").str == sinkModule &&
            row("sink_function_name").str == "setSysTime" &&
            row("sink_pc").num.toInt == 112 &&
            row("sink_trigger").str == "test.api.Process.forkExec" &&
            row("classification").str == "true-positive" &&
            row("path_steps").arr.nonEmpty &&
            row("path_steps").arr.forall(_.str.contains("::")) &&
            row("path_steps").arr.exists(_.str == s"$sourceModule::$sourceRef") &&
            row("path_steps").arr.exists(_.str == s"$sinkModule::$sinkRef") &&
            !row.obj.contains("callsite_id")
        )

        withClue("missing r8 setSysTime XQSysUtil forkExec path") {
          path.isDefined shouldBe true
        }
        val steps = path.get("path_steps").arr.map(_.str)
        steps should contain(s"$sinkModule::root.108@pc98:r3")
        steps should contain(s"$sinkModule::root.108@pc103:r3")
        steps should contain(s"$sinkModule::root.108@pc109:r0")
        steps should contain(s"$sinkModule::root.108@pc111:r4")
        steps should contain(s"$sinkModule::$sinkRef")
      }
    }

    "export CrossPlatform r8 webAccess path to XQSysUtil webAccessControl exec sink" in {
      withXiaomiStagingRows { stagingRows =>
        val sourceRows = stagingRows.flatMap(_("source_endpoints").arr.map(_.obj))
        val sinkRows   = stagingRows.flatMap(_("sink_endpoints").arr.map(_.obj))
        val pathRows   = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        val sourceModule = "usr/lib/lua/luci/controller/api/misystem.luac"
        val sinkModule   = "usr/lib/lua/xiaoqiang/util/XQSysUtil.luac"
        val sourceRef    = "root.136@pc17:r2"
        val bridgeRef    = "root.136@pc56:r7"
        val paramRef     = "root.105:r1"
        val sinkRef      = "root.105@pc32:r8"

        sourceRows.exists(row =>
          row("module_path").str == sourceModule &&
            row("value_ref").str == sourceRef &&
            hasScopedCallsite(row, "root.136@pc17") &&
            row("trigger").str == "luci.http.formvalue"
        ) shouldBe true

        sinkRows.exists(row =>
          row("module_path").str == sinkModule &&
            row("value_ref").str == sinkRef &&
            hasScopedCallsite(row, "root.105@pc32") &&
            row("trigger").str == "os.execute"
        ) shouldBe true

        val path = pathRows.find(row =>
          row("source_module_path").str == sourceModule &&
            row("source_function_name").str == "webAccess" &&
            row("source_pc").num.toInt == 17 &&
            row("source_trigger").str == "luci.http.formvalue" &&
            row("sink_module_path").str == sinkModule &&
            row("sink_function_name").str == "webAccessControl" &&
            row("sink_pc").num.toInt == 32 &&
            row("sink_trigger").str == "os.execute" &&
            row("classification").str == "true-positive" &&
            row("path_steps").arr.nonEmpty &&
            row("path_steps").arr.forall(_.str.contains("::")) &&
            row("path_steps").arr.exists(_.str == s"$sourceModule::$sourceRef") &&
            row("path_steps").arr.exists(_.str == s"$sourceModule::$bridgeRef") &&
            row("path_steps").arr.exists(_.str == s"$sinkModule::$paramRef") &&
            row("path_steps").arr.exists(_.str == s"$sinkModule::$sinkRef") &&
            !row.obj.contains("callsite_id")
        )

        withClue("missing r8 webAccess XQSysUtil webAccessControl os.execute path") {
          path.isDefined shouldBe true
        }
        val steps = path.get("path_steps").arr.map(_.str)
        steps should contain(s"$sinkModule::root.105@pc11:r6")
        steps should contain(s"$sinkModule::root.105@pc28:r6")
        steps should contain(s"$sinkModule::root.105@pc31:r8")
        steps should contain(s"$sinkModule::$sinkRef")
      }
    }

    "export CrossPlatform r8 pingTest path to luci sys exec sink" in {
      withXiaomiStagingRows { stagingRows =>
        val sourceRows = stagingRows.flatMap(_("source_endpoints").arr.map(_.obj))
        val sinkRows   = stagingRows.flatMap(_("sink_endpoints").arr.map(_.obj))
        val exportRows = stagingRows.flatMap(_("module_return_table").arr.map(_.obj))
        val linkRows   = stagingRows.flatMap(_("module_linkage").arr.map(_.obj))
        val pathRows   = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        val sourceModule = "usr/lib/lua/luci/controller/api/xqnetdetect.luac"
        val sinkModule   = "usr/lib/lua/luci/sys.luac"
        val sourceRef    = "root.3@pc6:r1"
        val bridgeRef    = "root.3@pc10:r3"
        val sinkRef      = "root.25@pc9:r2"

        sourceRows.exists(row =>
          row("module_path").str == sourceModule &&
            row("value_ref").str == sourceRef &&
            hasScopedCallsite(row, "root.3@pc6") &&
            row("trigger").str == "luci.http.formvalue"
        ) shouldBe true

        sinkRows.exists(row =>
          row("module_path").str == sinkModule &&
            row("value_ref").str == sinkRef &&
            hasScopedCallsite(row, "root.25@pc9") &&
            row("trigger").str == "os.execute"
        ) shouldBe true

        exportRows.exists(row =>
          row("module_path").str == sinkModule &&
            row("table_ref").str == s"$sinkModule:module-global" &&
            row("field_name").str == "net.pingtest" &&
            row("target_prototype_id").str == "root.25"
        ) shouldBe true

        linkRows.exists(row =>
          row("module_path").str == sourceModule &&
            hasScopedCallsite(row, "root.3@pc10") &&
            row("target_module_path").str == sinkModule &&
            row("target_prototype_id").str == "root.25" &&
            row("field_name").str == "net.pingtest"
        ) shouldBe true

        val path = pathRows.find(row =>
          row("source_module_path").str == sourceModule &&
            row("source_function_name").str == "pingTest" &&
            row("source_pc").num.toInt == 6 &&
            row("source_trigger").str == "luci.http.formvalue" &&
            row("sink_module_path").str == sinkModule &&
            row("sink_function_name").str == "net.pingtest" &&
            row("sink_pc").num.toInt == 9 &&
            row("sink_trigger").str == "os.execute" &&
            row("classification").str == "true-positive" &&
            row("path_steps").arr.nonEmpty &&
            row("path_steps").arr.forall(_.str.contains("::")) &&
            row("path_steps").arr.exists(_.str == s"$sourceModule::$sourceRef") &&
            row("path_steps").arr.exists(_.str == s"$sourceModule::$bridgeRef") &&
            row("path_steps").arr.exists(_.str == s"$sinkModule::$sinkRef") &&
            !row.obj.contains("callsite_id")
        )

        withClue("missing r8 pingTest luci.sys os.execute path") {
          path.isDefined shouldBe true
        }
        val steps = path.get("path_steps").arr.map(_.str)
        steps should contain(s"$sourceModule::root.3@pc9:r1")
        steps should contain(s"$sourceModule::$bridgeRef")
        steps should contain(s"$sinkModule::root.25:r0")
        steps should contain(s"$sinkModule::root.25@pc3:r4")
        steps should contain(s"$sinkModule::root.25@pc6:r4")
        steps should contain(s"$sinkModule::root.25@pc6:r3")
        steps should contain(s"$sinkModule::root.25@pc8:r2")
        steps should contain(s"$sinkModule::$sinkRef")
      }
    }

    "export CrossPlatform r8 miats remote_call paths to datacenter requestDatacenter" in {
      withXiaomiStagingRows { stagingRows =>
        val sourceRows = stagingRows.flatMap(_("source_endpoints").arr.map(_.obj))
        val sinkRows   = stagingRows.flatMap(_("sink_endpoints").arr.map(_.obj))
        val pathRows   = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        val sourceModule = "usr/lib/lua/luci/controller/api/miats.luac"
        val sinkModule   = "usr/lib/lua/luci/controller/service/datacenter.luac"
        val expectedSources = Vector(
          9  -> "root.9@pc9:r2",
          17 -> "root.9@pc17:r4"
        )

        expectedSources.foreach { case (pc, localRef) =>
          sourceRows.exists(row =>
            row("module_path").str == sourceModule &&
              row("value_ref").str == localRef &&
              hasScopedCallsite(row, s"root.9@pc$pc") &&
              row("trigger").str == "luci.http.formvalue"
          ) shouldBe true
        }

        sinkRows.exists(row =>
          row("module_path").str == sinkModule &&
            row("value_ref").str == "root.15@pc22:r6" &&
            hasScopedCallsite(row, "root.15@pc22") &&
            row("trigger").str == "luci.util.exec"
        ) shouldBe true

        val missingPaths = expectedSources.collect { case (pc, localRef)
            if !pathRows.exists(row =>
              row("source_module_path").str == sourceModule &&
                row("source_function_name").str == "remote_call" &&
                row("source_pc").num.toInt == pc &&
                row("source_trigger").str == "luci.http.formvalue" &&
                row("sink_module_path").str == sinkModule &&
                row("sink_function_name").str == "requestDatacenter" &&
                row("sink_pc").num.toInt == 22 &&
                row("sink_trigger").str == "luci.util.exec" &&
                row("path_steps").arr.nonEmpty &&
                row("path_steps").arr.forall(_.str.contains("::")) &&
                row("path_steps").arr.exists(_.str == s"$sourceModule::$localRef") &&
                row("path_steps").arr.exists(_.str == s"$sinkModule::root.15@pc22:r6") &&
                !row.obj.contains("callsite_id")
            ) =>
          s"miats.remote_call@pc$pc"
        }

        withClue(s"missing r8 miats requestDatacenter paths: ${missingPaths.mkString(", ")}") {
          missingPaths shouldBe empty
        }
      }
    }

    "export CrossPlatform r7 deleteTransportList path through XQBaiduPanUtil" in {
      withXiaomiStagingRows { stagingRows =>
        val pathRows = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        val path = pathRows
          .find(row =>
            row("source_module_path").str == "usr/lib/lua/luci/controller/api/xqnetwork.luac" &&
              row("source_function_name").str == "deleteTransportList" &&
              row("source_pc").num.toInt == 28 &&
              row("source_trigger").str == "luci.http.formvalue" &&
              row("sink_module_path").str == "usr/lib/lua/xiaoqiang/module/XQBaiduPanUtil.luac" &&
              row("sink_function_name").str == "kill_baidupan_process" &&
              row("sink_pc").num.toInt == 22 &&
              row("sink_trigger").str == "luci.util.exec" &&
              row("path_steps").arr.nonEmpty &&
              row("path_steps").arr.forall(_.str.contains("::")) &&
              !row.obj.contains("callsite_id")
          )
          .getOrElse(fail("missing deleteTransportList to XQBaiduPanUtil.kill_baidupan_process strict path"))

        val steps = path("path_steps").arr.map(_.str)
        steps should contain("usr/lib/lua/luci/controller/api/xqnetwork.luac::root.133@pc28:r8")
        steps should contain("usr/lib/lua/luci/controller/api/xqnetwork.luac::root.133@pc87:r15")
        steps should contain("usr/lib/lua/xiaoqiang/module/XQBaiduPanUtil.luac::root.39:r2")
        steps should contain("usr/lib/lua/xiaoqiang/module/XQBaiduPanUtil.luac::root.39@pc65:r9")
        steps should contain("usr/lib/lua/xiaoqiang/module/XQBaiduPanUtil.luac::root.39@pc75:r12")
        steps should contain("usr/lib/lua/xiaoqiang/module/XQBaiduPanUtil.luac::root.39@pc76:r14")
        steps should contain("usr/lib/lua/xiaoqiang/module/XQBaiduPanUtil.luac::root.37:r0")
        steps should contain("usr/lib/lua/xiaoqiang/module/XQBaiduPanUtil.luac::root.37@pc22:r3")
      }
    }

    "prune CrossPlatform captured-require bridge flows before local path search" in {
      withXiaomiExportDir { exportDir =>
        val profile = ujson.read(Files.readString(exportDir.resolve("path-search-profile.json"))).obj
        val pairProfiles = profile("performance_attribution")("pair_profiles").arr.map(_.obj)
        val targetPair = pairProfiles
          .find(row =>
            row("source_ref").str ==
              "usr/lib/lua/luci/controller/api/xqnetwork.luac:root.93@pc28:r8" &&
              row("source_callsite_id").str ==
                "usr/lib/lua/luci/controller/api/xqnetwork.luac::root.93@pc28" &&
              row("sink_ref").str ==
                "usr/lib/lua/xiaoqiang/common/XQFunction.luac:root.33@pc35:r4" &&
              row("sink_callsite_id").str ==
                "usr/lib/lua/xiaoqiang/common/XQFunction.luac::root.33@pc35"
          )
          .getOrElse(fail("missing attributed captured-require pair"))

        targetPair("taint_path_count").num.toLong shouldBe 1L
        targetPair("report_count").num.toLong shouldBe 1L
        targetPair("bridge_local_path_success_count").num.toLong shouldBe 6L
        targetPair("bridge_local_path_attempt_count").num.toLong should be <= 12L
      }
    }

    "export CrossPlatform r7 residual source-to-sink paths and miats sink endpoint" in {
      withXiaomiStagingRows { stagingRows =>
        val sinkRows = stagingRows.flatMap(_("sink_endpoints").arr.map(_.obj))
        val pathRows = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        def hasSink(moduleSuffix: String, pc: Int, trigger: String): Boolean =
          sinkRows.exists(row =>
            row("module_path").str.endsWith(moduleSuffix) &&
              row("callsite_id").str.endsWith(s"@pc$pc") &&
              row("callsite_id").str.contains("::") &&
              row("trigger").str == trigger
          )

        def hasPath(
          sourceModuleSuffix: String,
          sourceFunctionName: String,
          sourcePc: Int,
          sinkModuleSuffix: String,
          sinkFunctionName: String,
          sinkPc: Int,
          sinkTrigger: String
        ): Boolean =
          pathRows.exists(row =>
            row("source_module_path").str.endsWith(sourceModuleSuffix) &&
              row("source_function_name").str == sourceFunctionName &&
              row("source_pc").num.toInt == sourcePc &&
              row("source_trigger").str == "luci.http.formvalue" &&
              row("sink_module_path").str.endsWith(sinkModuleSuffix) &&
              row("sink_function_name").str == sinkFunctionName &&
              row("sink_pc").num.toInt == sinkPc &&
              row("sink_trigger").str == sinkTrigger &&
              row("path_steps").arr.nonEmpty &&
              row("path_steps").arr.forall(_.str.contains("::")) &&
              !row.obj.contains("callsite_id")
          )

        val missingFamilies = Vector(
          "miats.sink@pc148:luci.util.exec" -> hasSink(
            "usr/lib/lua/luci/controller/api/miats.luac",
            148,
            "luci.util.exec"
          ),
          "xqnetwork.setWifiApMode@pc28->XQFunction.nvramSet@pc35" -> hasPath(
            "usr/lib/lua/luci/controller/api/xqnetwork.luac",
            "setWifiApMode",
            28,
            "usr/lib/lua/xiaoqiang/common/XQFunction.luac",
            "nvramSet",
            35,
            "os.execute"
          ),
          "xqnetwork.setAllWifi@pc40->XQFunction.nvramSet@pc35" -> hasPath(
            "usr/lib/lua/luci/controller/api/xqnetwork.luac",
            "setAllWifi",
            40,
            "usr/lib/lua/xiaoqiang/common/XQFunction.luac",
            "nvramSet",
            35,
            "os.execute"
          ),
          "xqnetwork.setWifiApMode@pc28->XQSynchrodata.func_unknow_0_0@pc25" -> hasPath(
            "usr/lib/lua/luci/controller/api/xqnetwork.luac",
            "setWifiApMode",
            28,
            "usr/lib/lua/xiaoqiang/util/XQSynchrodata.luac",
            "func_unknow_0_0",
            25,
            "os.execute"
          ),
          "misystem.setWifiApMode_Init@pc60->XQSynchrodata.func_unknow_0_0@pc25" -> hasPath(
            "usr/lib/lua/luci/controller/api/misystem.luac",
            "setWifiApMode_Init",
            60,
            "usr/lib/lua/xiaoqiang/util/XQSynchrodata.luac",
            "func_unknow_0_0",
            25,
            "os.execute"
          ),
          "xqsmarthome.requestMitv@pc3->XQMitvUtil.DoExec@pc10" -> hasPath(
            "usr/lib/lua/luci/controller/api/xqsmarthome.luac",
            "requestMitv",
            3,
            "usr/lib/lua/xiaoqiang/util/XQMitvUtil.luac",
            "DoExec",
            10,
            "luci.util.exec"
          ),
          "xqnetwork.setWan6@pc40->xqnetwork.setWan6@pc276" -> hasPath(
            "usr/lib/lua/luci/controller/api/xqnetwork.luac",
            "setWan6",
            40,
            "usr/lib/lua/luci/controller/api/xqnetwork.luac",
            "setWan6",
            276,
            "test.api.Process.forkExec"
          ),
          "xqsystem.vpnSwitch@pc12->XQCryptoUtil.md5Str@pc10" -> hasPath(
            "usr/lib/lua/luci/controller/api/xqsystem.luac",
            "vpnSwitch",
            12,
            "usr/lib/lua/xiaoqiang/util/XQCryptoUtil.luac",
            "md5Str",
            10,
            "luci.util.exec"
          ),
          "xqnetwork.editDevice@pc20->XQWifiUtil.wl_editWiFiMacfilterList@pc348" -> hasPath(
            "usr/lib/lua/luci/controller/api/xqnetwork.luac",
            "editDevice",
            20,
            "usr/lib/lua/xiaoqiang/util/XQWifiUtil.luac",
            "wl_editWiFiMacfilterList",
            348,
            "os.execute"
          ),
          "xqsystem.ExtendWifiConnectInitedRouter@pc36->XQExtendWifi.write_t_v@pc31" -> hasPath(
            "usr/lib/lua/luci/controller/api/xqsystem.luac",
            "ExtendWifiConnectInitedRouter",
            36,
            "usr/lib/lua/xiaoqiang/module/XQExtendWifi.luac",
            "write_t_v",
            31,
            "os.execute"
          ),
          "xqnetwork.deleteTransportList@pc28->XQBaiduPanUtil.kill_baidupan_process@pc22" -> hasPath(
            "usr/lib/lua/luci/controller/api/xqnetwork.luac",
            "deleteTransportList",
            28,
            "usr/lib/lua/xiaoqiang/module/XQBaiduPanUtil.luac",
            "kill_baidupan_process",
            22,
            "luci.util.exec"
          )
        ).collect { case (label, false) => label }

        withClue(s"missing families: ${missingFamilies.mkString(", ")}") {
          missingFamilies shouldBe empty
        }
      }
    }

    "export CrossPlatform real-firmware sanitizer classifications from call-name rows" in {
      withXiaomiExportDir { exportDir =>
        val profile     = ujson.read(Files.readString(exportDir.resolve("path-search-profile.json"))).obj
        val stagingDir  = exportDir.resolve("staging")
        val stagingList = Files.list(stagingDir)
        try {
          val stagingRows = stagingList.iterator.asScala.toVector.map(path => ujson.read(Files.readString(path)).obj)
          val callRows    = stagingRows.flatMap(_("call_name_resolution").arr.map(_.obj))
          val pathRows    = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

          val sanitizerSuffixes = Set("tonumber", "tostring")
          callRows.exists(row =>
            row("module_path").str.endsWith("usr/lib/lua/luci/controller/api/misystem.luac") &&
              sanitizerSuffixes.contains(row("resolved_name").str) &&
              row.obj.contains("target_value_ref") &&
              row("target_value_ref").str.nonEmpty
          ) shouldBe true

          profile("sanitizer_classification_count").num.toInt should be > 0
          pathRows.exists(row => row("sanitizer_hits").arr.nonEmpty) shouldBe true
          pathRows.exists(row => row("classification").str == "sanitized") shouldBe true
          pathRows.exists(row => row("classification").str == "true-positive") shouldBe true
        } finally {
          stagingList.close()
        }
      }
    }

    "export CrossPlatform sanitizer call rows with original call argument refs" in {
      withXiaomiStagingRows { stagingRows =>
        val callRows = stagingRows.flatMap(_("call_name_resolution").arr.map(_.obj))

        val sanitizerRow = callRows
          .find(row =>
            row("module_path").str == "usr/lib/lua/luci/controller/api/xqnetwork.luac" &&
              row("callsite_id").str == "usr/lib/lua/luci/controller/api/xqnetwork.luac::root.93@pc101" &&
              row("resolved_name").str == "xiaoqiang.module.XQAPModule.setWifiAPMode" &&
              row("resolution_kind").str == "sanitizer-call"
          )
          .getOrElse(fail("missing setWifiAPMode sanitizer call row"))

        val argumentRefs = sanitizerRow("argument_value_refs").arr.map(_.str).toVector
        argumentRefs should contain("root.93@pc101:r29")
        argumentRefs should contain("root.93@pc101:r32")
        argumentRefs should not contain "root.93@pc101:r21"
      }
    }

    "reject CrossPlatform representative cross-module paths without source callsite bridge" in {
      withXiaomiStagingRows { stagingRows =>
        val pathRows = stagingRows.flatMap(_("path_evidence").arr.map(_.obj))

        pathRows.exists(row =>
          row("source_module_path").str.endsWith("usr/lib/lua/luci/controller/api/misystem.luac") &&
            row("source_pc").num.toInt == 27 &&
            row("sink_module_path").str.endsWith("usr/lib/lua/xiaoqiang/util/XQSynchrodata.luac") &&
            row("sink_pc").num.toInt == 25
        ) shouldBe false

        pathRows.exists(row =>
          row("source_module_path").str.endsWith("usr/lib/lua/luci/controller/api/misystem.luac") &&
            row("source_pc").num.toInt == 15 &&
            row("sink_module_path").str.endsWith("usr/lib/lua/xiaoqiang/util/XQQoSUtil.luac") &&
            row("sink_pc").num.toInt == 82 &&
            row("path_steps").arr.forall(_.str.contains("::"))
        ) shouldBe true
      }
    }

    "export CrossPlatform path search profile without repeated local graph builds" in {
      withXiaomiExportDir { exportDir =>
        val profile = ujson.read(Files.readString(exportDir.resolve("path-search-profile.json"))).obj

        profile.contains("local_path_graph_module_count") shouldBe true
        profile.contains("local_path_graph_build_count") shouldBe true
        profile.contains("local_path_search_count") shouldBe true

        val moduleCount = profile("local_path_graph_module_count").num.toInt
        val buildCount  = profile("local_path_graph_build_count").num.toInt
        val searchCount = profile("local_path_search_count").num.toInt

        moduleCount should be > 0
        searchCount should be > buildCount
        buildCount should be <= (moduleCount * 2)
      }
    }

    "export CrossPlatform path search profile with source-specific sink pruning" in {
      withXiaomiExportDir { exportDir =>
        val profile = ujson.read(Files.readString(exportDir.resolve("path-search-profile.json"))).obj

        profile.contains("source_sink_pair_count") shouldBe true
        profile.contains("qualified_source_sink_pair_count") shouldBe true
        profile.contains("prototype_pruned_source_sink_pair_count") shouldBe true
        profile.contains("distinct_local_path_query_count") shouldBe true

        val totalPairCount       = profile("source_sink_pair_count").num.toInt
        val qualifiedPairCount   = profile("qualified_source_sink_pair_count").num.toInt
        val prunedPairCount      = profile("prototype_pruned_source_sink_pair_count").num.toInt
        val distinctQueryCount   = profile("distinct_local_path_query_count").num.toInt
        val localPathSearchCount = profile("local_path_search_count").num.toInt

        totalPairCount shouldBe profile("source_endpoint_count").num.toInt * profile("sink_endpoint_count").num.toInt
        totalPairCount should be > qualifiedPairCount
        prunedPairCount shouldBe (totalPairCount - qualifiedPairCount)
        distinctQueryCount should be <= localPathSearchCount
        qualifiedPairCount should be > distinctQueryCount
      }
    }

    "export r7 performance attribution without changing legacy producer profile fields" in {
      withXiaomiExportDir { exportDir =>
        val profile = ujson.read(Files.readString(exportDir.resolve("path-search-profile.json"))).obj
        val legacyProfileKeys = Set(
          "status",
          "source_endpoint_count",
          "sink_endpoint_count",
          "taint_path_count",
          "sanitizer_classification_count",
          "report_count",
          "local_path_graph_module_count",
          "local_path_graph_build_count",
          "local_path_search_count",
          "distinct_local_path_query_count",
          "source_sink_pair_count",
          "qualified_source_sink_pair_count",
          "prototype_pruned_source_sink_pair_count"
        )

        profile.value.keySet shouldBe (legacyProfileKeys + "performance_attribution")
        profile("status").str shouldBe "completed"
        profile("source_sink_pair_count").num.toInt shouldBe
          profile("source_endpoint_count").num.toInt * profile("sink_endpoint_count").num.toInt
        profile("prototype_pruned_source_sink_pair_count").num.toInt shouldBe
          profile("source_sink_pair_count").num.toInt - profile("qualified_source_sink_pair_count").num.toInt

        val attribution = profile("performance_attribution").obj
        attribution("schema").str shouldBe "lua-r7-performance-attribution-v1"
        attribution("unattributed_changed_family_work").num.toLong shouldBe 0L

        val rows = attribution("rows").obj
        rows.value.keySet shouldBe Set("P1", "P2", "P3", "P4", "P5", "P6", "P7", "early_short_circuit")

        val p1 = rows("P1").obj
        p1("candidate_count").num.toLong should be > 0L
        p1("rejected_count").num.toLong should be > 0L
        p1("candidate_count").num.toLong shouldBe
          p1("rejected_count").num.toLong + p1("accepted_count").num.toLong

        val p7 = rows("P7").obj
        p7("status").str shouldBe "not-invoked-no-reuse"
        p7("invocation_count").num.toLong shouldBe 0L
        p7("reuse_count").num.toLong shouldBe 0L

        val p2 = rows("P2").obj
        p2("candidate_count").num.toLong shouldBe
          p2("accepted_count").num.toLong + p2("rejected_count").num.toLong
        val p3 = rows("P3").obj
        p3("candidate_count").num.toLong shouldBe
          p3("accepted_count").num.toLong + p3("prototype_rejected_count").num.toLong +
            p3("provenance_rejected_count").num.toLong
        (p2("rejected_count").num.toLong + p3("prototype_rejected_count").num.toLong +
          p3("provenance_rejected_count").num.toLong) should be > 0L
        val p4 = rows("P4").obj
        p4("candidate_count").num.toLong shouldBe
          p4("pc_rejected_count").num.toLong + p4("reachability_rejected_count").num.toLong +
            p4("continued_count").num.toLong
        p4("pc_rejected_count").num.toLong should be > 0L
        p4("reachability_rejected_count").num.toLong should be > 0L
        p4("path_constructor_candidate_count").num.toLong shouldBe
          p4("path_constructor_accepted_count").num.toLong + p4("path_constructor_rejected_count").num.toLong
        val p5 = rows("P5").obj
        (p5("node_visit_count").num.toLong + p5("edge_visit_count").num.toLong) should be > 0L
        val p6 = rows("P6").obj
        (p6("local_path_cache_hit_count").num.toLong + p6("local_path_cache_miss_count").num.toLong) should be > 0L
        val early = rows("early_short_circuit").obj
        early("count").num.toLong shouldBe
          early("pc_rejected_count").num.toLong + early("reachability_rejected_count").num.toLong
        early("count").num.toLong should be > 0L

        val pairProfiles = attribution("pair_profiles").arr.map(_.obj).toVector
        val pairCounterKeys = Vector(
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
        pairProfiles.foreach { row =>
          row("source_ref").str should not be empty
          row("sink_ref").str should not be empty
          row("source_callsite_id").str should include("::")
          row("sink_callsite_id").str should include("::")
          row("source_trigger").str should not be empty
          row("sink_trigger").str should not be empty
          row("pair_id").str shouldBe
            s"${row("source_ref").str}|${row("source_callsite_id").str}|${row("source_trigger").str}->" +
              s"${row("sink_ref").str}|${row("sink_callsite_id").str}|${row("sink_trigger").str}"
          pairCounterKeys.foreach(key => row(key).num.toLong should be >= 0L)
          row("source_reachability_check_count").num.toLong shouldBe
            row("source_reachability_accepted_count").num.toLong +
              row("prototype_unreachable_pair_count").num.toLong +
              row("source_specific_provenance_pruned_pair_count").num.toLong
          row("parameter_position_check_count").num.toLong shouldBe
            row("parameter_position_accepted_count").num.toLong + row("parameter_position_pruned_count").num.toLong
          row("path_constructor_check_count").num.toLong shouldBe
            row("path_constructor_accepted_count").num.toLong + row("path_constructor_pruned_count").num.toLong
          row("early_candidate_short_circuit_count").num.toLong shouldBe
            row("bridge_candidate_pc_pruned_count").num.toLong +
              row("bridge_candidate_reachability_pruned_count").num.toLong
          row("local_path_search_count").num.toLong shouldBe
            row("local_path_cache_hit_count").num.toLong + row("local_path_cache_miss_count").num.toLong
          row("distinct_local_path_query_count").num.toLong shouldBe row("local_path_cache_miss_count").num.toLong
          row("local_path_graph_build_count").num.toLong shouldBe row("local_path_graph_cache_miss_count").num.toLong
        }
        pairProfiles.map(_("pair_id").str).distinct.size shouldBe pairProfiles.size
        attribution("retained_pair_profile_count").num.toInt shouldBe pairProfiles.size
        attribution("retained_pair_profile_count").num.toLong should be <=
          profile("local_path_search_count").num.toLong + profile("taint_path_count").num.toLong
        attribution("retained_pair_profile_bytes").num.toLong shouldBe
          ujson.write(ujson.Arr.from(pairProfiles)).getBytes(StandardCharsets.UTF_8).length.toLong
        info(
          s"r7 retained_pair_profile_count=${pairProfiles.size} retained_pair_profile_bytes=${attribution("retained_pair_profile_bytes").num.toLong}"
        )
        pairProfiles.foreach { row =>
          (row("local_path_search_count").num.toLong > 0L || row("taint_path_count").num.toLong > 0L) shouldBe true
        }

        def selectPair(
          sourceRef: String,
          sourceCallsiteId: String,
          sourceTrigger: String,
          sinkRef: String,
          sinkCallsiteId: String,
          sinkTrigger: String
        ): ujson.Obj =
          pairProfiles
            .find(row =>
              row("source_ref").str == sourceRef &&
                row("source_callsite_id").str == sourceCallsiteId &&
                row("source_trigger").str == sourceTrigger &&
                row("sink_ref").str == sinkRef &&
                row("sink_callsite_id").str == sinkCallsiteId &&
                row("sink_trigger").str == sinkTrigger
            )
            .getOrElse(fail(s"missing attributed pair $sourceRef -> $sinkRef"))

        val commonPairs = Vector(
          selectPair("usr/lib/lua/luci/controller/api/misystem.luac:root.151@pc14:r3", "usr/lib/lua/luci/controller/api/misystem.luac::root.151@pc14", "luci.http.formvalue", "usr/lib/lua/xiaoqiang/common/XQFunction.luac:root.33@pc35:r4", "usr/lib/lua/xiaoqiang/common/XQFunction.luac::root.33@pc35", "os.execute"),
          selectPair("usr/lib/lua/luci/controller/api/xqsystem.luac:root.40@pc31:r11", "usr/lib/lua/luci/controller/api/xqsystem.luac::root.40@pc31", "luci.http.formvalue", "usr/lib/lua/xiaoqiang/common/XQFunction.luac:root.33@pc35:r4", "usr/lib/lua/xiaoqiang/common/XQFunction.luac::root.33@pc35", "os.execute"),
          selectPair("usr/lib/lua/luci/controller/api/misystem.luac:root.147@pc16:r4", "usr/lib/lua/luci/controller/api/misystem.luac::root.147@pc16", "luci.http.formvalue", "usr/lib/lua/luci/controller/api/misystem.luac:root.147@pc102:r9", "usr/lib/lua/luci/controller/api/misystem.luac::root.147@pc102", "os.execute")
        )
        commonPairs.foreach { row =>
          row("path_constructor_check_count").num.toLong should be > 0L
          row("local_path_search_count").num.toLong should be > 0L
          row("taint_path_count").num.toLong should be > 0L
          row("report_count").num.toLong should be > 0L
        }

        val unresolvedTargetPair =
          selectPair("usr/lib/lua/luci/controller/api/xqsmarthome.luac:root.5@pc3:r0", "usr/lib/lua/luci/controller/api/xqsmarthome.luac::root.5@pc3", "luci.http.formvalue", "usr/lib/lua/luci/util.luac:root.36@pc3:r2", "usr/lib/lua/luci/util.luac::root.36@pc3", "io.popen")
        unresolvedTargetPair("path_constructor_check_count").num.toLong should be > 0L
        unresolvedTargetPair("bridge_argument_provenance_candidate_count").num.toLong should be > 0L
        unresolvedTargetPair("taint_path_count").num.toLong shouldBe 0L
        unresolvedTargetPair("report_count").num.toLong shouldBe 0L

        val recoveredTargetPair =
          selectPair("usr/lib/lua/luci/controller/api/xqnetwork.luac:root.93@pc28:r8", "usr/lib/lua/luci/controller/api/xqnetwork.luac::root.93@pc28", "luci.http.formvalue", "usr/lib/lua/xiaoqiang/common/XQFunction.luac:root.33@pc35:r4", "usr/lib/lua/xiaoqiang/common/XQFunction.luac::root.33@pc35", "os.execute")
        recoveredTargetPair("path_constructor_check_count").num.toLong should be > 0L
        recoveredTargetPair("bridge_argument_provenance_candidate_count").num.toLong should be > 0L
        recoveredTargetPair("taint_path_count").num.toLong shouldBe 1L
        recoveredTargetPair("report_count").num.toLong shouldBe 1L
        Vector(unresolvedTargetPair, recoveredTargetPair).foreach { row =>
          row("path_constructor_check_count").num.toLong should be > 0L
          row("bridge_argument_provenance_candidate_count").num.toLong should be > 0L
        }
      }

    }

    "reject malformed r7 performance attribution before output creation" in {
      val requiredCounters = Vector(
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
      val counters = requiredCounters.map(_ -> 0L).toMap ++ Map(
        "source_reachability_check_count" -> 1L,
        "source_reachability_accepted_count" -> 1L,
        "parameter_position_check_count" -> 1L,
        "parameter_position_accepted_count" -> 1L,
        "path_constructor_check_count" -> 1L,
        "path_constructor_accepted_count" -> 1L,
        "local_path_search_count" -> 1L,
        "distinct_local_path_query_count" -> 1L,
        "local_path_cache_miss_count" -> 1L
      )
      val validRow = LuaPairPerformanceProfile(
        "a.luac:root@pc1:r0",
        "b.luac:root@pc2:r1",
        "a.luac::root@pc1",
        "b.luac::root@pc2",
        "luci.http.formvalue",
        "os.execute",
        counters
      )
      val baselineSemantics = LuaProgramSemantics(
        Vector.empty, Vector.empty, Vector.empty, Vector.empty, Vector.empty, Vector.empty, Vector.empty, Vector.empty,
        Vector.empty, Vector.empty, Vector.empty, Vector.empty, Vector.empty, Vector.empty, Vector.empty, Vector.empty,
        LuaPathSearchStats(0, 0, 1, 1, 1, 1, 0),
        LuaPerformanceAttribution(1, 0, 1, 0, Vector(validRow), counters)
      )
      val validSemantics = baselineSemantics.copy(
        sourceEndpoints = Vector(LuaSourceEndpoint(validRow.sourceRef, "a.luac:root@pc1:r0", validRow.sourceTrigger, "bytecode-only")),
        sinkEndpoints = Vector(LuaSinkEndpoint(validRow.sinkRef, "b.luac:root@pc2:r1", validRow.sinkTrigger, 0, "bytecode-only"))
      )
      def withAggregate(updated: Map[String, Long]): LuaProgramSemantics =
        validSemantics.copy(
          performanceAttribution = validSemantics.performanceAttribution.copy(aggregateCounters = updated)
        )
      val secondRow = validRow.copy(
        sourceRef = "c.luac:root@pc3:r0",
        sinkRef = "d.luac:root@pc4:r1",
        sourceCallsiteId = "c.luac::root@pc3",
        sinkCallsiteId = "d.luac::root@pc4"
      )
      val malformed = Vector(
        validSemantics.copy(performanceAttribution = validSemantics.performanceAttribution.copy(pairProfiles = Vector.empty)) -> "pair profiles are empty",
        withAggregate(counters - "report_count") -> "aggregate counter keys do not exactly match",
        withAggregate(counters + ("unknown_count" -> 1L)) -> "aggregate counter keys do not exactly match",
        validSemantics.copy(performanceAttribution = validSemantics.performanceAttribution.copy(p1CandidateCount = 2L)) -> "P1 candidate count does not partition",
        withAggregate(counters.updated("parameter_position_check_count", 2L)) -> "parameter position partition mismatch for aggregate",
        withAggregate(counters.updated("source_reachability_check_count", 2L)) -> "source reachability partition mismatch for aggregate",
        withAggregate(counters.updated("bridge_candidate_pc_pruned_count", 1L)) -> "bridge candidate partition mismatch for aggregate",
        validSemantics.copy(pathSearchStats = validSemantics.pathSearchStats.copy(localPathSearchCount = 2)) -> "aggregate local path search count does not match legacy count",
        validSemantics.copy(performanceAttribution = validSemantics.performanceAttribution.copy(pairProfiles = Vector(validRow.copy(counters = counters.updated("taint_path_count", 1L))))) -> "path reconciliation mismatch",
        validSemantics.copy(performanceAttribution = validSemantics.performanceAttribution.copy(pairProfiles = Vector(validRow.copy(counters = counters.updated("report_count", 1L))))) -> "report reconciliation mismatch",
        validSemantics.copy(performanceAttribution = validSemantics.performanceAttribution.copy(pairProfiles = Vector(validRow, secondRow))) -> "retained pair profile count exceeds",
        validSemantics.copy(performanceAttribution = validSemantics.performanceAttribution.copy(pairProfiles = Vector(validRow.copy(sourceTrigger = "x" * 5000)))) -> "retained pair profile payload exceeds",
        validSemantics.copy(performanceAttribution = validSemantics.performanceAttribution.copy(pairProfiles = Vector(validRow, validRow))) -> "pair identities are not unique",
        validSemantics.copy(performanceAttribution = validSemantics.performanceAttribution.copy(pairProfiles = Vector(validRow.copy(counters = counters - "report_count")))) -> "counter keys do not exactly match",
        validSemantics.copy(performanceAttribution = validSemantics.performanceAttribution.copy(pairProfiles = Vector(validRow.copy(counters = counters + ("unknown_count" -> 1L))))) -> "counter keys do not exactly match",
        validSemantics.copy(performanceAttribution = validSemantics.performanceAttribution.copy(pairProfiles = Vector(validRow.copy(sourceCallsiteId = "b.luac::root@pc1")))) -> "mismatched source identity",
        validSemantics.copy(performanceAttribution = validSemantics.performanceAttribution.copy(pairProfiles = Vector(validRow.copy(sourceTrigger = "")))) -> "empty trigger",
        validSemantics.copy(performanceAttribution = validSemantics.performanceAttribution.copy(pairProfiles = Vector(validRow.copy(counters = counters.updated("path_constructor_accepted_count", 0L))))) -> "path constructor partition mismatch"
      )

      malformed.foreach { case (semantics, expectedMessage) =>
        FileUtil.usingTemporaryDirectory("lua2cpg-invalid-r7-attribution") { tmpDir =>
          val exportDir = tmpDir.resolve("must-not-exist")
          val error = intercept[IllegalStateException](LuaRealFirmwareEvidenceExporter.write(
            Config(realFirmwareOutputDir = Some(exportDir.toString)),
            Vector.empty,
            semantics
          ))
          error.getMessage should include(expectedMessage)
          Files.exists(exportDir) shouldBe false
        }
      }
    }

    "export CrossPlatform upvalue closure call targets for source-specific pruning" in {
      withXiaomiStagingRows { stagingRows =>
        val synchrodata = stagingRows
          .find(_("relative_path").str.endsWith("usr/lib/lua/xiaoqiang/util/XQSynchrodata.luac"))
          .getOrElse(fail("missing XQSynchrodata staging evidence"))

        val targetRows = synchrodata("call_target_candidate").arr.map(_.obj)
        targetRows.exists(row =>
          hasScopedCallsite(row, "root.3@pc6") &&
            row("target_ref").str.endsWith("usr/lib/lua/xiaoqiang/util/XQSynchrodata.luac::root.0") &&
            row("resolution_status").str == "matched"
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

  private def hasScopedCallsite(row: ujson.Obj, localCallsiteId: String): Boolean =
    row("callsite_id").str.contains("::") && row("callsite_id").str.endsWith(s"::$localCallsiteId")

  private def assertRequestMitvStringMatchSanitizer(path: ujson.Obj): Unit = {
    val sanitizerCall = "usr/lib/lua/xiaoqiang/util/XQMitvUtil.luac::root.1@pc36"
    path("classification").str shouldBe "sanitized"
    path("path_steps").arr.exists(_.str == s"$sanitizerCall:r3") shouldBe true
    path("sanitizer_hits").arr.exists { hit =>
      val row = hit.obj
      row("callsite_id").str == sanitizerCall &&
        row("sanitizer_name").str == "string.match" &&
        row("applies_to_sink").bool &&
        row("on_dataflow_chain").bool
    } shouldBe true
  }

  private def withXiaomiStagingRows(test: Vector[ujson.Obj] => Unit): Unit = {
    withXiaomiExportDir { exportDir =>
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

  private def withXiaomiExportDir(test: java.nio.file.Path => Unit): Unit = {
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

      test(exportDir)
    }
  }
}
