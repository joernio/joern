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

        val targetPairs = Vector(
          selectPair("usr/lib/lua/luci/controller/api/xqsmarthome.luac:root.5@pc3:r0", "usr/lib/lua/luci/controller/api/xqsmarthome.luac::root.5@pc3", "luci.http.formvalue", "usr/lib/lua/luci/util.luac:root.36@pc3:r2", "usr/lib/lua/luci/util.luac::root.36@pc3", "io.popen"),
          selectPair("usr/lib/lua/luci/controller/api/xqnetwork.luac:root.93@pc28:r8", "usr/lib/lua/luci/controller/api/xqnetwork.luac::root.93@pc28", "luci.http.formvalue", "usr/lib/lua/xiaoqiang/common/XQFunction.luac:root.33@pc35:r4", "usr/lib/lua/xiaoqiang/common/XQFunction.luac::root.33@pc35", "os.execute")
        )
        targetPairs.foreach { row =>
          row("path_constructor_check_count").num.toLong should be > 0L
          row("bridge_argument_provenance_candidate_count").num.toLong should be > 0L
          row("taint_path_count").num.toLong shouldBe 0L
          row("report_count").num.toLong shouldBe 0L
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
