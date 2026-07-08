package io.joern.lua2cpg

import io.shiftleft.codepropertygraph.cpgloading.CpgLoader
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Paths

class RulesSanitizerReportSmokeTest extends AnyWordSpec with Matchers {

  "Lua2Cpg" should {
    "emit rules sanitizer and report markers that survive CPG reopen" in {
      val resourceRoot = Paths.get(getClass.getClassLoader.getResource("rules-sanitizer-report").toURI)

      FileUtil.usingTemporaryDirectory("lua2cpg-rules-sanitizer-report-smoke") { tmpDir =>
        val outputPath = tmpDir.resolve("rules-sanitizer-report.cpg.bin").toString
        val cpg = new Lua2Cpg()
          .createCpg(Config().withInputPath(resourceRoot.toString).withOutputPath(outputPath))
          .get
        cpg.close()

        val reopened = CpgLoader.load(outputPath)
        try {
          markerCodes(reopened, "lua.rule.match") should contain allOf (
            "d16-rf-formvalue-os-execute-chain/input.luac:root@pc16 formvalue -> luci.http.formvalue",
            "d16-rf-formvalue-os-execute-chain/input.luac:root@pc20 execute -> os.execute",
            "d16-rf-webcmd-cross-module-popen/mtkwifi.luac:root.1@pc3 popen -> io.popen"
          )
          markerCodes(reopened, "lua.source.endpoint") should contain allOf (
            "d16-rf-formvalue-os-execute-chain/input.luac:root@pc16:r0 via luci.http.formvalue",
            "d16-rf-webcmd-cross-module-popen/controller.luac:root.1@pc4:r0 via luci.http.formvalue"
          )
          markerCodes(reopened, "lua.sink.endpoint") should contain allOf (
            "d16-rf-formvalue-os-execute-chain/input.luac:root@pc20:r2 via os.execute param=0",
            "d16-rf-webcmd-cross-module-popen/mtkwifi.luac:root.1@pc3:r2 via io.popen param=0"
          )

          markerCodes(reopened, "lua.sanitizer.call") should contain allOf (
            "d24-sanitizer-suppresses-report/input.luac:root@pc20 tonumber -> d24-sanitizer-suppresses-report/input.luac:root@pc20:r2",
            "d24-sanitizer-same-suffix-off-chain-negative/input.luac:root@pc21 tonumber -> d24-sanitizer-same-suffix-off-chain-negative/input.luac:root@pc21:r3"
          )
          markerCodes(reopened, "lua.sanitizer.classification") should contain allOf (
            "d24-sanitizer-suppresses-report/input.luac:root@pc17:r1 -> d24-sanitizer-suppresses-report/input.luac:root@pc24:r4 classification=sanitized sanitizer=tonumber",
            "d24-sanitizer-same-suffix-off-chain-negative/input.luac:root@pc17:r1 -> d24-sanitizer-same-suffix-off-chain-negative/input.luac:root@pc25:r4 classification=not-sanitized sanitizer=tonumber"
          )
          markerCodes(reopened, "lua.report.classification") should contain allOf (
            "d24-sanitizer-suppresses-report/input.luac:root@pc17:r1 -> d24-sanitizer-suppresses-report/input.luac:root@pc24:r4 classification=sanitized reason=on-chain-sanitizer",
            "d24-sanitizer-same-suffix-off-chain-negative/input.luac:root@pc17:r1 -> d24-sanitizer-same-suffix-off-chain-negative/input.luac:root@pc25:r4 classification=true-positive reason=no-on-chain-sanitizer"
          )
          markerCodes(reopened, "lua.report.vulnerability") should contain allOf (
            "d16-rf-formvalue-os-execute-chain/input.luac:root@pc16:r0 -> d16-rf-formvalue-os-execute-chain/input.luac:root@pc20:r2 status=path-proven classification=true-positive path=d16-rf-formvalue-os-execute-chain/input.luac:root@pc16:r0;d16-rf-formvalue-os-execute-chain/input.luac:root@pc19:r2;d16-rf-formvalue-os-execute-chain/input.luac:root@pc20:r2",
            "d16-rf-submit-dpp-uri-execute/input.luac:root.2@pc4:r0 -> d16-rf-submit-dpp-uri-execute/input.luac:root.2@pc8:r2 status=path-proven classification=true-positive path=d16-rf-submit-dpp-uri-execute/input.luac:root.2@pc4:r0;d16-rf-submit-dpp-uri-execute/input.luac:root.2@pc7:r2;d16-rf-submit-dpp-uri-execute/input.luac:root.2@pc8:r2",
            "d16-rf-webcmd-cross-module-popen/controller.luac:root.1@pc4:r0 -> d16-rf-webcmd-cross-module-popen/mtkwifi.luac:root.1@pc3:r2 status=path-proven classification=true-positive path=d16-rf-webcmd-cross-module-popen/controller.luac:root.1@pc4:r0;d16-rf-webcmd-cross-module-popen/controller.luac:root.1@pc8:r2;d16-rf-webcmd-cross-module-popen/mtkwifi.luac:root.1@pc3:r2"
          )
          markerCodes(reopened, "lua.e5.boundary") should contain allOf (
            "d24-rules-overmatch-constant-sink-negative/input.luac:root@pc4 reason=rule-overmatch-rejected",
            "d24-rules-overmatch-constant-sink-negative/input.luac:root@pc8 reason=rule-overmatch-rejected",
            "d24-rules-overmatch-constant-sink-negative/input.luac:root@pc12 reason=fixed-string-sink-suppressed",
            "d24-report-no-report-without-path-negative/input.luac:source-to-sink reason=endpoint-only-no-path",
            "bc-kill-overwrite/input.luac:root@pc3:r2->root@pc7:r4 reason=killed-taint-path",
            "bc-branch-negative/input.luac:root@pc3:r2->root@pc8:r5 reason=branch-negative-taint-path"
          )

          val ruleCodes = markerCodes(reopened, "lua.rule.match")
          ruleCodes.exists(_.contains("formvaluex")) shouldBe false
          ruleCodes.exists(_.contains("executex")) shouldBe false
          markerCodes(reopened, "lua.sink.endpoint")
            .exists(_.contains("d24-rules-overmatch-constant-sink-negative")) shouldBe false

          markerCodes(reopened, "lua.report.vulnerability")
            .exists(_.contains("d24-sanitizer-suppresses-report")) shouldBe false
          markerCodes(reopened, "lua.report.vulnerability")
            .exists(_.contains("d24-report-no-report-without-path-negative")) shouldBe false
          markerCodes(reopened, "lua.report.vulnerability")
            .exists(code => code.contains("bc-kill-overwrite") || code.contains("bc-branch-negative")) shouldBe false

          val e5NodeCount = reopened.call
            .name("lua\\.(rule\\.match|source\\.endpoint|sink\\.endpoint|sanitizer\\.call|sanitizer\\.classification|report\\.classification|report\\.vulnerability|e5\\.boundary)")
            .size
          val reportCount = reopened.call.nameExact("lua.report.vulnerability").size
          info(s"e5_node_count=$e5NodeCount")
          info(s"e5_report_count=$reportCount")
          e5NodeCount should be > 0
          reportCount should be > 0
        } finally {
          reopened.close()
        }
      }
    }
  }

  private def markerCodes(cpg: io.shiftleft.codepropertygraph.generated.Cpg, name: String): List[String] =
    cpg.call.nameExact(name).code.l
}
