package io.joern.joerncli

import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.layers.dataflows.OssDataFlow
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.joern.x2cpg.layers.{Base, CallGraph, ControlFlow, TypeRelations}
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.testing.MockCpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CpgBasedToolTests extends AnyWordSpec with Matchers {

  "addDataFlowOverlayIfNonExistent" should {

    "apply the default overlays before the data flow overlay" in {
      implicit val semantics: Semantics = DefaultSemantics()

      // A CPG as written by a language frontend CLI has no overlays applied at all.
      val cpg = MockCpg().withMetaData(Languages.C, List.empty).cpg
      cpg.metaData.overlays.l shouldBe empty

      CpgBasedTool.addDataFlowOverlayIfNonExistent(cpg)

      // The call graph overlay carries MethodRefLinker, which creates the mandatory
      // METHOD_REF -REF-> METHOD edges that the data flow passes dereference. Without it
      // ReachingDefPass fails on any CPG containing a closure-captured METHOD_REF.
      val overlays = cpg.metaData.overlays.l
      overlays should contain(Base.overlayName)
      overlays should contain(CallGraph.overlayName)
      overlays should contain(OssDataFlow.overlayName)
    }

    "not re-apply overlays that are already present" in {
      implicit val semantics: Semantics = DefaultSemantics()

      val existing = List(Base.overlayName, ControlFlow.overlayName, TypeRelations.overlayName, CallGraph.overlayName)
      val cpg      = MockCpg().withMetaData(Languages.C, existing).cpg

      CpgBasedTool.addDataFlowOverlayIfNonExistent(cpg)

      val overlays = cpg.metaData.overlays.l
      overlays.count(_ == Base.overlayName) shouldBe 1
      overlays should contain(OssDataFlow.overlayName)
    }
  }
}
