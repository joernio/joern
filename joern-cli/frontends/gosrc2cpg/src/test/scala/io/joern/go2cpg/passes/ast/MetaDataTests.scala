package io.joern.go2cpg.passes.ast

import io.joern.x2cpg.layers.{Base, CallGraph, ControlFlow, TypeRelations}
import io.shiftleft.codepropertygraph.generated.Languages
import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.semanticcpg.language.*

class MetaDataTests extends GoCodeToCpgSuite {

  private val cpg = code("""
      |package main
      |func foo() {}
      |""".stripMargin)

  "should contain exactly one node with all mandatory fields set" in {
    val List(x) = cpg.metaData.l
    x.language shouldBe Languages.GOLANG
    x.version shouldBe "0.1"
    x.overlays shouldBe List(
      Base.overlayName,
      ControlFlow.overlayName,
      TypeRelations.overlayName,
      CallGraph.overlayName
    )
    // Go-frontend does not set hash for entire CPG.
    // Change this assertion if it is supposed to.
    x.hash shouldBe None
  }

  "should not have any incoming or outgoing edges" in {
    cpg.metaData.size shouldBe 1
    cpg.metaData.in.l shouldBe List()
    cpg.metaData.out.l shouldBe List()
  }

}
