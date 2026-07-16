package io.joern.lua2cpg

import io.shiftleft.codepropertygraph.cpgloading.CpgLoader
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Paths

class InterproceduralModuleTaintSmokeTest extends AnyWordSpec with Matchers {

  "Lua2Cpg" should {
    "emit interprocedural module and taint markers that survive CPG reopen" in {
      val resourceRoot = Paths.get(getClass.getClassLoader.getResource("interprocedural-module-taint").toURI)

      FileUtil.usingTemporaryDirectory("lua2cpg-interprocedural-module-taint-smoke") { tmpDir =>
        val outputPath = tmpDir.resolve("interprocedural-module-taint.cpg.bin").toString
        val cpg = new Lua2Cpg()
          .createCpg(Config().withInputPath(resourceRoot.toString).withOutputPath(outputPath))
          .get
        cpg.close()

        val reopened = CpgLoader.load(outputPath)
        try {
          markerCodes(reopened, "lua.interproc.arg_flow") should contain(
            "d16-rf-interprocedural-formvalue-execute/input.luac:root@pc18:r4 -> d16-rf-interprocedural-formvalue-execute/input.luac:root.3:r0"
          )
          markerCodes(reopened, "lua.interproc.arg_flow") should contain(
            "d24-interproc-unresolved-callee-negative/input.luac:root@pc8:r6 -> d24-interproc-unresolved-callee-negative/input.luac:root.2:r1"
          )
          markerCodes(reopened, "lua.interproc.return_flow") should contain(
            "d16-rf-interprocedural-formvalue-execute/input.luac::root.2@pc4:r0 -> d16-rf-interprocedural-formvalue-execute/input.luac:root@pc15:r2"
          )
          markerCodes(reopened, "lua.module.resolution") should contain(
            "d16-rf-webcmd-cross-module-popen/controller.luac require mtkwifi -> matched:d16-rf-webcmd-cross-module-popen/mtkwifi.luac"
          )
          markerCodes(reopened, "lua.module.return_table") should contain(
            "d24-module-return-table-field-call/library.luac::run -> root.0"
          )
          markerCodes(reopened, "lua.module.field_call_target") should contain allOf (
            "d24-module-return-table-field-call/controller.luac:root.0@pc10 -> d24-module-return-table-field-call/library.luac::root.0",
            "d24-module-return-table-field-call/controller.luac:root.1@pc10 -> d24-module-return-table-field-call/library.luac::root.0"
          )
          markerCodes(reopened, "lua.calltarget.cross_boundary") should contain(
            "d16-rf-webcmd-cross-module-popen/controller.luac:root.1@pc8 -> d16-rf-webcmd-cross-module-popen/mtkwifi.luac::root.1"
          )
          markerCodes(reopened, "lua.taint.path") should contain(
            "bc-taint-minimal-path/input.luac:root@pc3:r2 -> bc-taint-minimal-path/input.luac:root@pc6:r4 via bc-taint-minimal-path/input.luac:root@pc3:r2;bc-taint-minimal-path/input.luac:root@pc5:r4;bc-taint-minimal-path/input.luac:root@pc6:r4"
          )

          val unresolvedArgFlows = markerCodes(reopened, "lua.interproc.arg_flow")
            .filter(_.contains("d24-interproc-unresolved-callee-negative"))
          withClue(s"unresolved arg flows: ${unresolvedArgFlows.mkString(", ")}") {
            unresolvedArgFlows.exists(_.contains("root.2@pc2")) shouldBe false
          }
          val unresolvedReturnFlows = markerCodes(reopened, "lua.interproc.return_flow")
            .filter(_.contains("d24-interproc-unresolved-callee-negative"))
          withClue(s"unresolved return flows: ${unresolvedReturnFlows.mkString(", ")}") {
            unresolvedReturnFlows.exists(_.contains("root.2@pc2")) shouldBe false
          }
          markerCodes(reopened, "lua.module.resolution")
            .exists(code =>
              code.contains("d24-module-ambiguous-unresolved-dynamic-negative") && code.contains("-> matched:")
            ) shouldBe false
          markerCodes(reopened, "lua.calltarget.cross_boundary")
            .exists(_.contains("d24-module-missing-field-negative")) shouldBe false
          markerCodes(reopened, "lua.taint.path")
            .exists(code => code.contains("bc-kill-overwrite") || code.contains("bc-branch-negative")) shouldBe false

          val boundaryCodes = markerCodes(reopened, "lua.e4.boundary")
          boundaryCodes should contain allOf (
            "d24-interproc-unresolved-callee-negative/input.luac:root.2@pc2 reason=unresolved-callee",
            "d24-module-ambiguous-unresolved-dynamic-negative/missing.luac:require:missing.module reason=unresolved-module",
            "d24-module-ambiguous-unresolved-dynamic-negative/ambiguous.luac:require:shared.module reason=ambiguous-module",
            "d24-module-ambiguous-unresolved-dynamic-negative/controller.luac:require:dynamic reason=dynamic-require",
            "d24-module-missing-field-negative/controller.luac:root.0@pc3 reason=missing-export-field",
            "bc-kill-overwrite/input.luac:root@pc3:r2->root@pc7:r4 reason=killed-taint-path",
            "bc-branch-negative/input.luac:root@pc3:r2->root@pc8:r5 reason=branch-negative-taint-path"
          )

          val e4NodeCount = reopened.call
            .name("lua\\.(module\\.resolution|module\\.return_table|module\\.field_call_target|interproc\\.arg_flow|interproc\\.return_flow|calltarget\\.cross_boundary|taint\\.path|e4\\.boundary)")
            .size
          val e4ReachingDefEdgeCount = reopened.identifier.outE(EdgeTypes.REACHING_DEF).size
          val e4TaintPathCount       = reopened.call.nameExact("lua.taint.path").size
          info(s"e4_node_count=$e4NodeCount")
          info(s"e4_reaching_def_edge_count=$e4ReachingDefEdgeCount")
          info(s"e4_taint_path_count=$e4TaintPathCount")
          e4NodeCount should be > 0
          e4ReachingDefEdgeCount should be > 0
          e4TaintPathCount should be > 0
        } finally {
          reopened.close()
        }
      }
    }
  }

  private def markerCodes(cpg: io.shiftleft.codepropertygraph.generated.Cpg, name: String): List[String] =
    cpg.call.nameExact(name).code.l
}
