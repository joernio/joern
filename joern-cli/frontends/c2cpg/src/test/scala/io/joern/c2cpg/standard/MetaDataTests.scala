package io.joern.c2cpg.standard

import io.joern.c2cpg.testfixtures.CCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.language._
import io.joern.x2cpg.layers.{Base, CallGraph, ControlFlow, TypeRelations}
import overflowdb.traversal._

class MetaDataTests extends CCodeToCpgSuite {

  private val cpg = code("""
      |
      |""".stripMargin)

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
