package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.joern.x2cpg.layers.Base
import io.joern.x2cpg.layers.CallGraph
import io.joern.x2cpg.layers.ControlFlow
import io.joern.x2cpg.layers.TypeRelations
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.language.*

class MetaDataTests extends Rust2CpgSuite(noSysRoot = true) {

  private val cpg = code("")

  "should contain exactly one node with all mandatory fields set" in {
    val List(x) = cpg.metaData.l
    x.language shouldBe Languages.RUST
    x.version should not equal "0.1"
    x.overlays shouldBe List(
      Base.overlayName,
      ControlFlow.overlayName,
      TypeRelations.overlayName,
      CallGraph.overlayName
    )
  }

  "should not have any incoming or outgoing edges" in {
    cpg.metaData.size shouldBe 1
    cpg.metaData.in.l shouldBe List()
    cpg.metaData.out.l shouldBe List()
  }
}
