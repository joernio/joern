package io.joern.lua2cpg

import io.shiftleft.codepropertygraph.cpgloading.CpgLoader
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNode, StoredNode}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Paths

class IntraproceduralSemanticsSmokeTest extends AnyWordSpec with Matchers {

  "Lua2Cpg" should {
    "emit intraprocedural bytecode semantics that survive CPG reopen" in {
      val resourceRoot = Paths.get(getClass.getClassLoader.getResource("intraprocedural-semantics").toURI)

      FileUtil.usingTemporaryDirectory("lua2cpg-intraprocedural-semantics-smoke") { tmpDir =>
        val outputPath = tmpDir.resolve("intraprocedural-semantics.cpg.bin").toString
        val cpg = new Lua2Cpg()
          .createCpg(Config().withInputPath(resourceRoot.toString).withOutputPath(outputPath))
          .get
        cpg.close()

        val reopened = CpgLoader.load(outputPath)
        try {
          hasReachingDef(reopened, "bc-kill-overwrite", "root@pc4:r2", "root@pc7:r4") shouldBe true
          hasReachingDef(reopened, "d24-defuse-transitive-chain", "root@pc3:r2", "root@pc8:r6") shouldBe true
          hasReachingDef(reopened, "local-value-flow-generic", "root.0:r0", "root.0@pc6:r4") shouldBe true
          hasReachingDef(reopened, "local-value-flow-generic", "root.2:r0", "root.2@pc6:r3") shouldBe true

          hasReachingDef(reopened, "bc-kill-overwrite", "root@pc3:r2", "root@pc7:r4") shouldBe false
          hasReachingDef(reopened, "local-value-flow-generic", "root.1:r0", "root.1@pc6:r3") shouldBe false
          hasReachingDef(
            reopened,
            "d24-defuse-unrelated-register-negative",
            "root@pc4:r2",
            "root@pc9:r6"
          ) shouldBe false
          hasReachingDef(reopened, "d24-table-dynamic-key-negative", "root@pc3:r2", "root@pc4:r3") shouldBe false
          hasReachingDef(reopened, "d24-table-dynamic-key-negative", "root@pc3:r2", "root@pc5:r4") shouldBe false
          hasReachingDef(reopened, "d24-global-dynamic-env-negative", "root@pc3:r1", "root@pc4:r2") shouldBe false
          hasReachingDef(reopened, "d24-global-dynamic-env-negative", "root@pc3:r1", "root@pc5:r3") shouldBe false
          hasReachingDef(reopened, "d24-upvalue-mutation-negative", "root.0@pc0:r0", "root.0@pc3:r1") shouldBe false

          hasSemanticEdge(
            reopened,
            "bc-table-global-upvalue",
            "table:root.0@pc3:r1:root.0:k0",
            "root.0@pc1:r0",
            "root.0@pc3:r3"
          ) shouldBe true
          hasReachingDef(reopened, "bc-table-global-upvalue", "root.0@pc6:r3", "root.0@pc8:r2") shouldBe true
          hasSemanticEdge(
            reopened,
            "bc-table-global-upvalue",
            "upvalue:root.0:u0",
            "root.0@pc4:r4",
            "root.0@pc4:r4"
          ) shouldBe true

          reopened.call
            .nameExact("lua.calltarget.candidate")
            .codeExact("root@pc5 -> root.1")
            .nonEmpty shouldBe true
          reopened.call
            .nameExact("lua.calltarget.unresolved")
            .codeExact("root.1@pc2 unresolved=param-derived")
            .nonEmpty shouldBe true
          reopened.call
            .nameExact("lua.calltarget.candidate")
            .code(".*source-function-name.*")
            .isEmpty shouldBe true

          val expectedBoundaries = Seq(
            "bc-kill-overwrite:no-tainted-source-after-overwrite",
            "d24-defuse-unrelated-register-negative:no-defuse-cross-prototype-register-reuse",
            "d24-table-dynamic-key-negative:no-table-field-flow-dynamic-key",
            "d24-table-dynamic-key-negative:no-table-field-flow-missing-field",
            "d24-global-dynamic-env-negative:no-global-flow-dynamic-env-write",
            "d24-global-dynamic-env-negative:no-global-flow-missing-precise-name",
            "d24-upvalue-mutation-negative:no-stale-upvalue-reuse-after-setupval",
            "d24-upvalue-mutation-negative:upvalue-mutation-boundary",
            "bc-call-candidate-unresolved:no-guessed-source-target"
          )
          val actualBoundaries = reopened.call.nameExact("lua.semantic.boundary").code.l.toSet
          expectedBoundaries.diff(actualBoundaries.toSeq).toList.shouldBe(Nil)

          val nodeCount        = reopened.graph.allNodes.size
          val reachingDefCount = reopened.identifier.outE(EdgeTypes.REACHING_DEF).size
          (nodeCount > 0) shouldBe true
          (reachingDefCount > 0) shouldBe true
          info(s"intraprocedural-semantics node_count=$nodeCount reaching_def_edge_count=$reachingDefCount")
        } finally {
          reopened.close()
        }
      }
    }
  }

  private def hasReachingDef(
    cpg: io.shiftleft.codepropertygraph.generated.Cpg,
    fixtureId: String,
    sourceCode: String,
    sinkCode: String
  ): Boolean =
    hasSemanticEdge(cpg, fixtureId, sourceCode, sourceCode, sinkCode) || transitiveReachingDef(
      cpg,
      fixtureId,
      sourceCode,
      sinkCode
    )

  private def hasSemanticEdge(
    cpg: io.shiftleft.codepropertygraph.generated.Cpg,
    fixtureId: String,
    variable: String,
    sourceCode: String,
    sinkCode: String
  ): Boolean = {
    val fixtureNodes = semanticNodesInFixture(cpg, fixtureId)
    val sinkIds = fixtureNodes
      .filter(_.code == sinkCode)
      .map(_.id)
      .toSet

    fixtureNodes
      .filter(_.code == sourceCode)
      .outE(EdgeTypes.REACHING_DEF)
      .filter(edge => Option(edge.property).contains(variable))
      .exists { edge =>
        val sink = edge.dst.asInstanceOf[StoredNode]
        sinkIds.contains(sink.id)
      }
  }

  private def transitiveReachingDef(
    cpg: io.shiftleft.codepropertygraph.generated.Cpg,
    fixtureId: String,
    sourceCode: String,
    sinkCode: String
  ): Boolean = {
    val fixtureNodes = semanticNodesInFixture(cpg, fixtureId)
    val fixtureNodeCodes = fixtureNodes
      .map(identifier => identifier.id -> identifier.code)
      .toMap

    val graph = fixtureNodes
      .outE(EdgeTypes.REACHING_DEF)
      .flatMap { edge =>
        val sourceNode = edge.src.asInstanceOf[StoredNode]
        val sinkNode   = edge.dst.asInstanceOf[StoredNode]
        val source     = fixtureNodeCodes.get(sourceNode.id)
        val sink       = fixtureNodeCodes.get(sinkNode.id)
        source.zip(sink).headOption
      }
      .foldLeft(Map.empty[String, Set[String]]) { case (acc, (source, sink)) =>
        acc.updated(source, acc.getOrElse(source, Set.empty) + sink)
      }
    val seen = scala.collection.mutable.Set.empty[String]
    val work = scala.collection.mutable.Stack(sourceCode)
    while (work.nonEmpty) {
      val current = work.pop()
      if (current == sinkCode) {
        return true
      }
      if (!seen(current)) {
        seen += current
        graph.getOrElse(current, Set.empty).diff(seen.toSet).foreach(work.push)
      }
    }
    false
  }

  private def semanticNodesInFixture(
    cpg: io.shiftleft.codepropertygraph.generated.Cpg,
    fixtureId: String
  ): List[CfgNode] =
    cpg.method.filename(s".*$fixtureId/input\\.luac").parameter.l ++
      cpg.method.filename(s".*$fixtureId/input\\.luac").ast.isIdentifier.l

}
