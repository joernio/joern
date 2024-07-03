package io.joern.rubysrc2cpg.deprecated.passes.ast

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.joern.x2cpg.layers.{Base, CallGraph, ControlFlow, TypeRelations}
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.language.*
class MetaDataTests extends RubyCode2CpgFixture(useDeprecatedFrontend = true) {
  val cpg = code("""puts 123""")

  "should contain exactly one node with all mandatory fields set" in {
    val List(x) = cpg.metaData.l
    x.language shouldBe Languages.RUBYSRC
    x.version shouldBe "0.1"
    x.overlays shouldBe List(
      Base.overlayName,
      ControlFlow.overlayName,
      TypeRelations.overlayName,
      CallGraph.overlayName
    )
    x.hash shouldBe None
  }

  "should not have any incoming or outgoing edges" in {
    cpg.metaData.size shouldBe 1
    cpg.metaData.in.l shouldBe List()
    cpg.metaData.out.l shouldBe List()
  }
}
