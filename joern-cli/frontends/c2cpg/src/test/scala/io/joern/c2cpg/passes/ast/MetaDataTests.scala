package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.x2cpg.layers.Base
import io.joern.x2cpg.layers.CallGraph
import io.joern.x2cpg.layers.ControlFlow
import io.joern.x2cpg.layers.TypeRelations
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.language.*

class MetaDataTests extends C2CpgSuite {

  private val cpg = code("")

  "should contain exactly one node with all mandatory fields set" in {
    val List(x) = cpg.metaData.l
    x.language shouldBe Languages.NEWC
    x.version shouldBe "0.1"
    x.overlays shouldBe List(
      Base.overlayName,
      ControlFlow.overlayName,
      TypeRelations.overlayName,
      CallGraph.overlayName
    )
    // C-frontend does not set hash for entire CPG.
    // Change this assertion if it is supposed to.
    x.hash shouldBe None
  }

  "should not have any incoming or outgoing edges" in {
    cpg.metaData.size shouldBe 1
    cpg.metaData.in.l shouldBe List()
    cpg.metaData.out.l shouldBe List()
  }

}
